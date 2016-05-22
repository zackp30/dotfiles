import           System.Exit
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.Fullscreen
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.NoBorders
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.Shell
import           XMonad.Util.EZConfig
import           XMonad.Util.Run(hPutStrLn, spawnPipe)

modKey = mod4Mask


main = do
    xmproc <- spawnPipe "~/.local/bin/xmobar ~/.xmobarrc"
    xmonad $ defaultConfig
       {
          terminal = "urxvt-256color"
        , manageHook = myManageHook <+> fullscreenManageHook <+> manageDocks
                       <+> manageHook defaultConfig
        , modMask = modKey
        , layoutHook = fullscreenFloat $ fullscreenFocus $ layoutHints
                       $ smartBorders $ avoidStruts $ layoutHook defaultConfig
        , handleEventHook = fullscreenEventHook <+> docksEventHook
        , logHook = dynamicLogWithPP $ xmobarPP { ppOutput = hPutStrLn xmproc }
       }
       `additionalKeysP`
         [
            -- Ask for confirmation before exiting by replacing the default
            -- binding with a custom one.
            ("M-S-q", confirmPrompt myXPConfig "exit" $ io exitSuccess)
            -- Replace M-p launcher binding with built-in shellPrompt, which
            -- is friendlier and does not require dmenu.
          , ("M-p", shellPrompt myXPConfig)
         ]
       `additionalKeys`
        [ ((modKey, xK_q), spawn "xmonad --recompile && xmonad --restart") ]

-- see http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Prompt.html#t:XPConfig
myXPConfig = defaultXPConfig
    {
      position = Top
    , promptBorderWidth = 0
    , defaultText = ""
    , alwaysHighlight = True
    , font = "9x15"
    }

-- see http://xmonad.org/xmonad-docs/xmonad/XMonad-ManageHook.html
myManageHook = composeAll
    [className =? "mpv"                --> doFloat]
