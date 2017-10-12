import           Codec.Binary.UTF8.String
import qualified Data.Map as M
import           System.Exit
import           System.IO
import           XMonad
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig

import           XMonad.Actions.SpawnOn
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.NoBorders
import           XMonad.Layout.ToggleLayouts
import           XMonad.Util.Loggers
import           XMonad.Util.Run


-- colors
blue  = "#08c"
black = "#222"

myWorkspaces = map show [1..9]

-- keysbindings

myKeys = \c -> mkKeymap c $
  [ ("M-<Return>", spawn $ terminal c)
  , ("M-c"       , kill)
  , ("M-p"       , spawn "dmenu_run")
  , ("M-<Space>" , sendMessage NextLayout)
  , ("M-j"       , windows W.focusDown)
  , ("M-k"       , windows W.focusUp)
  , ("M-S-k"     , windows W.swapUp)
  , ("M-S-j"     , windows W.swapDown)
  , ("M-h"       , sendMessage Shrink)
  , ("M-l"       , sendMessage Expand)
  , ("M-t"       , withFocused $ windows . W.sink)
  , ("M-,"       , sendMessage (IncMasterN 1))
  , ("M-."       , sendMessage (IncMasterN (-1)))
  , ("M-S-q"     , io (exitWith ExitSuccess))
  , ("M-q"       , spawn "xmonad --recompile; xmonad --restart")
  , ("M-f"       , sendMessage ToggleStruts >> sendMessage (Toggle "Full"))
  , ("<XF86AudioMute>"         , spawn "pa-adjust mute")
  , ("<XF86AudioLowerVolume>"  , spawn "pa-adjust minus")
  , ("<XF86AudioRaiseVolume>"  , spawn "pa-adjust plus")
  , ("<XF86MonBrightnessDown>" , spawn "xbacklight -dec 15")
  , ("<XF86MonBrightnessUp>"   , spawn "xbacklight -inc 15")
  , ("<XF86PowerOff>"          , spawn "slock")
  , ("<XF86Eject>"             , spawn "slock")
  , ("<XF86AudioStop>"         , spawn "mpc stop")
  , ("<XF86AudioPlay>"         , spawn "mpc toggle")
  , ("<XF86AudioNext>"         , spawn "mpc next")
  , ("<XF86AudioPrev>"         , spawn "mpc prev")
  , ("<XF86Tools>"        , spawn "mpc clear; mpc load all")
  , ("M-A-l"                 , spawn "slock")
  ]
  ++
  [("M-" ++ [k], windows $ W.greedyView i)
  | (i, k) <- zip (XMonad.workspaces c) ['1' .. '9']
  ]
  ++
  [("M-S-" ++ [k], windows $ W.shift i)
  | (i, k) <- zip (XMonad.workspaces c) ['1' .. '9']
  ]
  ++
  [("M-" ++ key, screenWorkspace sc >>= flip whenJust (windows . W.view))
  | (key, sc) <- zip ["w", "e", "r"] [0..]
  ]

-- Status Bar

bar = "lemonbar -d -f \"DejaVu Sans Mono:size=10\" -f \"FontAwesome-10\" -u 6 -B \"#1D1F21\" -F\"#DDD\""

lemonbarColor :: String -> String -> String -> String
lemonbarColor fg bg s = concat ["%{F", fg, "}%{B", bg, "}", s, "%{B-}%{F-}"]

lemonbarFG :: String -> String -> String
lemonbarFG fg s = concat ["%{F", fg, "}", s, "%{F-}"]

lemonbarBG :: String -> String -> String
lemonbarBG bg s = concat ["%{B", bg, "}", s, "%{B-}"]

-- log formatting
myLogHook h = dynamicLogWithPP defaultPP
  { ppCurrent = const $ lemonbarFG "#191B1D" "\xf111"
  , ppWsSep = " "
  , ppSep = ""
  , ppVisible = const $ lemonbarFG "#191B1D" "\xf10c"
  -- , ppUrgent = lemonbarColor "-" "#dc322f" . wrap " " " "
  , ppHidden = const $ lemonbarFG "#191B1D" "\xf10c"
  , ppHiddenNoWindows = const $ lemonbarFG "#444" "\xf10c"
  , ppTitle = const ""
  , ppLayout = const ""
  , ppOrder = \out ->
      case out of
        (ws:_:_:v:d:t:_) ->
          [ lemonbarBG "#333" (wrap " " " " ws)
          , "%{r}"
          , wrap (lemonbarFG "#569FE0" " \xf028 ") "% " v
          -- , wrap (lemonbarFG "#B05A5A" " \xf004 ") " " b
          , lemonbarBG "#333" (wrap (lemonbarFG "#C2CF4C" " \xf073 ") " " d)
          , wrap (lemonbarFG "#C2CF4C" " \xf017 ") " " t
          ]
        o -> o
  , ppExtras = [ logCmd "pamixer --get-volume"
               , date "%a, %b %d"
               , date "%I:%M"
               , battery
               ]
  , ppOutput = hPutStrLn h
  }

myLayout = toggleLayouts Full (Tall 1 (3/100) (1/2))

myManageHook = composeAll
  [ manageDocks
  , manageHook defaultConfig
  ]

main = do
  h <- spawnPipe bar
  xmonad def
    { normalBorderColor  = black
    , focusedBorderColor = blue
    , modMask            = mod4Mask
    , terminal           = "urxvt"
    , keys               = myKeys
    , borderWidth        = 1
    , workspaces         = myWorkspaces
    , layoutHook         = avoidStruts . smartBorders $ myLayout
    , handleEventHook    = fullscreenEventHook <+> docksEventHook <+> handleEventHook defaultConfig
    , manageHook         = myManageHook
    , logHook            = myLogHook h
    }
