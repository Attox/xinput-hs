module Main where

import           Data.List.Split
import qualified Data.Text                 as DT
import           Data.Maybe
import           System.Process
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.ModelView as Model


data Device = Device { name     :: String
                     , deviceId :: String
                     , status   :: String
                     , values :: [String]
                     } deriving (Show, Eq)

formatName :: String -> String
formatName = unwords . dropWhile ((==) 1 . length) . words

formatId :: String -> String
formatId id = last $ splitOn "=" id

parseDevices :: String -> [[String]]
parseDevices deviceList =
  map (map (DT.unpack . DT.strip . DT.pack) . splitOn "\t")
    $ splitOn "\n" deviceList

createDevRecord :: [String] -> Device
createDevRecord deviceEntry = Device {name = a, deviceId = b, status = status, values= v}
 where
  (name : id : status : rest) = deviceEntry
  a                           = formatName name
  b                           = formatId id
  v = [""]

getDevices :: IO [Device]
getDevices = do
  n <- readProcess "xinput" ["--list", "--short"] []
  return $ map createDevRecord . parseDevices $ init n

getProperties :: Device -> IO Device
getProperties Device { name=n, status=s, deviceId=i} = do
  props <- readProcess "xinput" ["list-props", i] []
  let properties = map (DT.unpack . DT.strip . DT.pack) $ lines props
  return $ Device {name=n, status=s, deviceId=i, values=properties }

onSelection :: ListStore Device -> Model.TreeSelection -> ComboBox -> IO ()
onSelection list tree textview = do
  -- clear combox
  n <- comboBoxGetModelText textview
  listStoreClear n
  comboBoxSetModelText textview
  -- populate with properties of selected device
  sel <- Model.treeSelectionGetSelectedRows tree
  let s = head (head sel)
  v     <- Model.listStoreGetValue list s
  props <- getProperties v
  mapM_ (comboBoxAppendText textview . DT.pack) $ values props

populateLabels :: ComboBoxText -> String -> (String, String)
populateLabels properties device =
  let n      = splitOn "\t" (DT.unpack properties)
      propId = take 3 . tail . last . words . head $ n
  in  (device, propId)

setLabels :: Entry -> Entry -> (String, String) -> IO ()
setLabels d p (deviceL, propL) = do
  entrySetText d deviceL
  entrySetText p propL

currentPropertyIds :: ListStore Device -> TreeSelection -> ComboBox -> Entry -> Entry -> IO ()
currentPropertyIds list tree textbox deviceL propL = do
  sel         <- Model.treeSelectionGetSelectedRows tree
  selProperty <- comboBoxGetActiveText textbox
  v           <- Model.listStoreGetValue list (head (head sel))
  props       <- getProperties v
  case selProperty of
    Nothing -> return ()
    _       -> setLabels deviceL propL
      $ populateLabels (fromJust selProperty) (deviceId props)

run :: Entry -> Entry -> Entry -> IO ()
run devId propId newVal = do
  d       <- entryGetText devId :: IO String
  p       <- entryGetText propId :: IO String
  v       <- entryGetText newVal :: IO String
  procOut <- readProcess "xinput" ["set-prop", d, p, v] []
  print procOut

main :: IO ()
main = do
  -- get our device list
  n <- getDevices
  -- setup basic UI elements
  initGUI
  window         <- windowNew
  vbox           <- vBoxNew False 10
  hbox           <- hBoxNew True 10
  devLabel       <- labelNew (Just "Device ID:")
  propLabel      <- labelNew (Just "Property ID:")
  valLabel       <- labelNew (Just "new Value:")
  changeDevice   <- entryNew
  changeProperty <- entryNew
  newValue       <- entryNew
  commitChange   <- buttonNewWithLabel "Change!"
  list           <- listStoreNew n --store this
  combobox       <- comboBoxNewText
  treeview       <- Model.treeViewNewWithModel list
  scrwinTree     <- scrolledWindowNew Nothing Nothing
  -- treview that stores all our devices
  col            <- Model.treeViewColumnNew -- column for our content
  renderer       <- Model.cellRendererTextNew
  Model.cellLayoutPackStart     col renderer True
  Model.cellLayoutSetAttributes col renderer list
    $ \d -> [Model.cellText := name d ++ "\t ID:" ++ deviceId d]
  Model.treeViewAppendColumn treeview col
  tree <- Model.treeViewGetSelection treeview
  Model.treeSelectionSetMode    tree       SelectionSingle
  -- embed treeview in scrolled windows
  scrolledWindowAddWithViewport scrwinTree treeview
  scrolledWindowSetPolicy scrwinTree PolicyAutomatic PolicyAutomatic
  --  events
  Model.onSelectionChanged tree (onSelection list tree combobox)
  commitChange `on` buttonActivated $ run changeDevice changeProperty newValue
  combobox `on` changed $ currentPropertyIds list
                                             tree
                                             combobox
                                             changeDevice
                                             changeProperty
  -- create containers
  set
    window
    [ windowDefaultWidth := 800
    , windowDefaultHeight := 600
    , windowResizable := True
    , containerBorderWidth := 10
    , containerChild := vbox
    ]
  boxPackStart vbox combobox       PackNatural 0
  boxPackStart vbox scrwinTree     PackGrow    0
  boxPackStart hbox devLabel       PackGrow    0
  boxPackStart hbox changeDevice   PackGrow    0
  boxPackStart hbox propLabel      PackGrow    0
  boxPackStart hbox changeProperty PackGrow    0
  boxPackStart hbox valLabel       PackGrow    0
  boxPackStart hbox newValue       PackGrow    0
  boxPackStart hbox commitChange   PackGrow    0
  boxPackStart vbox hbox           PackNatural 0
  -- run GUI
  onDestroy window mainQuit
  widgetShowAll window
  mainGUI
