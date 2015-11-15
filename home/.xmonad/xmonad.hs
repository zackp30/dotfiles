import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Actions.GridSelect
import XMonad.Layout.Accordion
import System.IO

myMod = mod1Mask
myLayout = tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100
myTerminal = "urxvt-256color"

main = do
  xmproc <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $ defaultConfig {
         logHook = dynamicLogWithPP xmobarPP
                         { ppOutput = hPutStrLn xmproc,
                           ppTitle = xmobarColor "green" "" . shorten 50
                         },
         manageHook = manageDocks <+> manageHook defaultConfig,
         layoutHook = avoidStruts myLayout,
         terminal = myTerminal,
         focusedBorderColor = "#03C5E7"
    } `additionalKeys`
    [ ((myMod, xK_BackSpace), goToSelected defaultGSConfig)
    , ((myMod .|. shiftMask, xK_n), sendMessage NextLayout)
    , ((myMod .|. shiftMask, xK_p), sendMessage FirstLayout)
    , ((myMod, xK_Return), spawn myTerminal)
    , ((myMod, xK_p), spawn "rofi -show run")]
