module Main
where

import           Data.List.Split
import qualified Data.Text                     as DT
import           Data.Maybe
import           Control.Monad.IO.Class
import           System.Process
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Builder


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
createDevRecord deviceEntry = Device
  { name     = a
  , deviceId = b
  , status   = status
  , values   = [""]
  }
 where
  (name : id : status : rest) = deviceEntry
  a                           = formatName name
  b                           = formatId id

getDevices :: IO [Device]
getDevices = do
  n <- readProcess "xinput" ["--list", "--short"] []
  return $ map createDevRecord . parseDevices $ init n

getProperties :: Device -> IO Device
getProperties Device { name = n, status = s, deviceId = i } = do
  props <- readProcess "xinput" ["list-props", i] []
  let properties = map (DT.unpack . DT.strip . DT.pack) $ lines props
  return $ Device {name = n, status = s, deviceId = i, values = properties}

onSelection :: ListStore Device -> TreeSelection -> ComboBox -> IO ()
onSelection list tree combobox = do
  -- populate with properties of selected device
  sel <- treeSelectionGetSelectedRows tree
  let s = head (head sel)
  v     <- listStoreGetValue list s
  props <- getProperties v
  comboBoxSetModelText combobox
  mapM_ (comboBoxAppendText combobox . DT.pack) (values props)

parseLabels :: ComboBoxText -> String -> (String, String)
parseLabels properties device =
  let n      = splitOn "\t" (DT.unpack properties)
      propId = take 3 . tail . last . words . head $ n
  in  (device, propId)

setLabels :: Entry -> Entry -> (String, String) -> IO ()
setLabels d p (deviceL, propL) = do
  entrySetText d deviceL
  entrySetText p propL

getDeviceInformation
  :: ListStore Device -> TreeSelection -> ComboBox -> Entry -> Entry -> IO ()
getDeviceInformation list tree textbox deviceL propL = do
  sel         <- treeSelectionGetSelectedRows tree
  selProperty <- comboBoxGetActiveText textbox
  v           <- listStoreGetValue list (head (head sel))
  props       <- getProperties v
  case selProperty of
    Nothing -> return ()
    _       -> setLabels deviceL propL
      $ parseLabels (fromJust selProperty) (deviceId props)

run :: Entry -> Entry -> Entry -> IO ()
run devId propId newVal = do
  d       <- entryGetText devId
  p       <- entryGetText propId
  v       <- entryGetText newVal
  procOut <- readProcess "xinput" ["set-prop", d, p, v] []
  print procOut

main :: IO ()
main = do
  -- get our device list and init builder
  initGUI
  n       <- getDevices
  builder <- builderNew
  builderAddFromFile builder "./gui.glade"
  -- setup basic UI elements
  list           <- listStoreNew n --store this
  window         <- builderGetObject builder castToWindow "window1"
  devLabel       <- builderGetObject builder castToLabel "label1"
  propLabel      <- builderGetObject builder castToLabel "label2"
  valLabel       <- builderGetObject builder castToLabel "label3"
  changeDevice   <- builderGetObject builder castToEntry "entry1"
  changeProperty <- builderGetObject builder castToEntry "entry2"
  newValue       <- builderGetObject builder castToEntry "entry3"
  commitChange   <- builderGetObject builder castToButton "button1"
  treeview       <- builderGetObject builder castToTreeView "treeview1"
  combobox       <- builderGetObject builder castToComboBox "combobox1"
  -- treview that stores all our devices
  treeViewSetModel treeview list
  col      <- treeViewColumnNew -- column for our content
  renderer <- cellRendererTextNew
  cellLayoutPackStart col renderer True
  cellLayoutSetAttributes col renderer list
    $ \d -> [cellText := name d ++ "\t ID:" ++ deviceId d]
  treeViewAppendColumn treeview col
  tree <- treeViewGetSelection treeview
  treeSelectionSetMode tree SelectionSingle
  --  events
  on treeview cursorChanged (onSelection list tree combobox)
  commitChange `on` buttonActivated $ run changeDevice changeProperty newValue
  combobox `on` changed $ getDeviceInformation list
                                               tree
                                               combobox
                                               changeDevice
                                               changeProperty
  -- run GUI
  window `on` deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window
  mainGUI
