module Main
where

import           Data.List.Split
import qualified Data.Text                     as DT
import           Data.Maybe
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
  , values   = v
  }
 where
  (name : id : status : rest) = deviceEntry
  a                           = formatName name
  b                           = formatId id
  v                           = [""]

getDevices :: IO [Device]
getDevices = do
  n <- readProcess "xinput" ["--list", "--short"] []
  return $ map createDevRecord . parseDevices $ init n

getProperties :: Device -> IO Device
getProperties Device { name = n, status = s, deviceId = i } = do
  props <- readProcess "xinput" ["list-props", i] []
  let properties = map (DT.unpack . DT.strip . DT.pack) $ lines props
  return $ Device {name = n, status = s, deviceId = i, values = properties}

onSelection :: ListStore Device -> TreeSelection -> ListStore DT.Text -> IO ()
onSelection list tree propertyList = do
  -- clear combox
  listStoreClear propertyList
  -- populate with properties of selected device
  sel <- treeSelectionGetSelectedRows tree
  let s = head (head sel)
  v     <- listStoreGetValue list s
  props <- getProperties v
  mapM_ (listStoreAppend propertyList. DT.pack) $ values props

populateLabels :: ComboBoxText -> String -> (String, String)
populateLabels properties device =
  let n      = splitOn "\t" (DT.unpack properties)
      propId = take 3 . tail . last . words . head $ n
  in  (device, propId)

setLabels :: Entry -> Entry -> (String, String) -> IO ()
setLabels d p (deviceL, propL) = do
  entrySetText d deviceL
  entrySetText p propL

currentPropertyIds
  :: ListStore Device -> TreeSelection -> ComboBox -> Entry -> Entry -> IO ()
currentPropertyIds list tree textbox deviceL propL = do
  sel         <- treeSelectionGetSelectedRows tree
  selProperty <- comboBoxGetActiveText textbox
  v           <- listStoreGetValue list (head (head sel))
  props       <- getProperties v
  case selProperty of
    Nothing -> return ()
    _       -> setLabels deviceL propL
      $ populateLabels (fromJust selProperty) (deviceId props)

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
  box <- builderGetObject builder castToBox "box3"
  combobox       <- comboBoxNewText
  text <- comboBoxGetModelText combobox
  treeview       <- builderGetObject builder castToTreeView "treeview1"
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
  on treeview cursorChanged (onSelection list tree text)
  commitChange `on` buttonActivated $ run changeDevice changeProperty newValue
  combobox `on` changed $ currentPropertyIds list
                                             tree
                                             combobox
                                             changeDevice
                                             changeProperty
                                             
  boxPackStart box combobox PackNatural 0
  -- run GUI
  widgetShowAll window
  mainGUI
