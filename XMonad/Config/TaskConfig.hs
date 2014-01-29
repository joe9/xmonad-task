
{-# OPTIONS_GHC -Wall #-}
module XMonad.Config.TaskConfig
   ( taskConfig
   , startupTasks
   -- , allMyTaskNames
   -- , myTaskConfig
   , nonEmptyRecentsOnCurrentScreen
   , nonEmptyTags
   , tasks
   ) where

-- system imports
import           Control.Monad
import           Data.List                        (lookup)
import           Data.Map                         (Map, fromList,
                                                   union)
import           Safe

-- xmonad core
import           XMonad
import           XMonad.StackSet                  hiding (workspaces)
import qualified XMonad.StackSet                  as S

-- xmonad contrib
import           XMonad.Actions.Commands
import           XMonad.Actions.CycleRecentWS
import           XMonad.Actions.CycleWindows
import           XMonad.Actions.GridSelect
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.ServerMode
import qualified XMonad.Layout.BoringWindows      as B
import           XMonad.Layout.IndependentScreens
import           XMonad.Layout.WindowNavigation

import           XMonad.Actions.Task
import           XMonad.Config.XmobarConfig
import           XMonad.Hooks.TaskCommands

taskConfig =
  desktopConfig
    { handleEventHook       =
         serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
            <+> serverModeTaskEventHookCmd' taskCommands defaultCommands
            <+> dynStatusBarEventHook
                 barCreator
                 barDestroyer
    -- , workspaces  = startupTaskWorkspaces nScreens startupTasks
    , workspaces  = startupTaskWorkspaces 2 startupTasks
    , logHook     = tasksPP pp >>= (\p -> multiPP p p)
    , terminal    = myTerminal
    , keys        = liftM2 union taskKeyBindings (keys def)
    }

-- http://hpaste.org/51334 -- hoschi config
-- http://hpaste.org/52657 -- byorgey config
-- The list of all tasks/workspaces of your xmonad configuration.
-- The order is important, new tasks must be inserted
-- at the end of the list if you want hot-restarting
-- to work.
-- define some custom tasks for use with the TaskSpace module.
startupTasks, tasks :: [Task]
startupTasks = myTaskdefinitions
tasks = myTaskdefinitions

-- | Do not use underscores in Task Names. Underscore is used as a
-- delimiter by IndependentScreens and this module
myTaskdefinitions :: [Task]
myTaskdefinitions =
    (  concat
        . replicate 3
        . map (\s -> Task "terminal" (S s) "/home/j/" Nothing 0)
        $ [0..1])
    ++   concatMap f   [ srcDir
                      , "/home/j/etc/xmonad"
                      , "/home/j/etc/zsh"
                      , "/home/j/etc/X11"
                      , "/home/j/etc/emacs/emacs.d"
                      ]
    ++   map ff1   [ "/home/j/Desktop/"
                        , "/home/j/etc/emacs/emacs.d"
                        , "/home/j/"
                        , "/home/j/"
                        , "/var/log"
                        ]
    ++   map ff0 [ "/home/j/" ]
  where
      snsrDir   = "/home/j/dev/apps/pic/standalone/"
      srcDir    = snsrDir ++ "src/all"
      f d = map (\s -> Task "terminal" (S s) d (Just "Full") 0) [0..1]
      ff1 d = Task "terminal" (S 1) d (Just "Full") 0
      ff0 d = Task "terminal" (S 0) d (Just "Full") 0

-- got this from
-- http://m1.archiveorange.com/
--    m/att/j8BnA/ArchiveOrange_C872rLadnB1wa3dTcZztmNk4KA8a.hs

-- Build a list of windowsets with current swapped in turn with each
-- "most recent" workspace as given by nonEmptyTags
nonEmptyRecentsOnCurrentScreen ::
   StackSet String l a ScreenId sd
   -> [StackSet String l a ScreenId sd]
nonEmptyRecentsOnCurrentScreen ws =
      -- map (view `flip` ws)
          -- (rotUp . Prelude.filter (\x -> isOnScreen x ws)
          --         $ nonEmptyTags ws)
      -- map (view `flip` ws) (rotUp $ nonEmptyTags ws)
      map (view `flip` ws)
          (rotUp . filterNonCurrentScreenWorkspaces $ nonEmptyTags ws)
   where currentScreen  = screen . current $ ws
         filterNonCurrentScreenWorkspaces =
            Prelude.filter (\x -> fst (unmarshall x) == currentScreen)
         -- filterNonCurrentScreenWorkspaces =
         --    Prelude.filter (\x -> isOnScreen (view x ws) ws)

-- Given a windowset grab a list of the workspace tags,
--    in the default order
-- current, visibles in screen order,
--    hiddens from most to least recently accessed.
nonEmptyTags ::  StackSet String l a s sd -> [String]
nonEmptyTags ws =
   [ wtag | Workspace wtag _ (Just _) <- S.workspaces ws ]


taskKeyBindings :: XConfig l -> Map (KeyMask, KeySym) (X ())
taskKeyBindings conf = fromList $
   [ ((m .|. shiftMask, xK_Return), spawnShell            )

   -- ("M-S-<Tab>",
   --    cycleRecentWS' [xK_Super_L, xK_Shift_L] xK_Tab xK_grave)
   -- I use mod-tab to start the action, and as soon as mod is
   --    released it ends the cycling that is, as long as I am
   --    holding down mod I can keep hitting tab to keep cycling
   --    back through less recent workspaces
   --    so you can hold down mod-shift (or whatever) and hit tab,
   --    tab, tab,... whoops I missed the workspace I wanted! ...
   --    type ` ... release.
   , ((m, xK_Tab), cycleWindowSets nonEmptyRecentsOnCurrentScreen
                                   [xK_space]
                                   xK_Tab
                                   xK_grave)

   -- %! Move focus to the previous window
   -- %! Move focus to the master window
   , ((m,               xK_a   ), B.focusMaster)
    -- window navigation keybindings.
   , ((m,               xK_k   ), sendMessage $ Go U)
   , ((m,               xK_j   ), sendMessage $ Go D)
   , ((m,               xK_l   ), sendMessage $ Go R)
   , ((m,               xK_h   ), sendMessage $ Go L)

   -- for workspace 0
   , ((m,               xK_w   ), withScreen 0 view)
   , ((m .|. shiftMask, xK_w   ), withScreen 0 viewShift)
   , ((m,               xK_e   ), withScreen 1 view)
   , ((m .|. shiftMask, xK_e   ), withScreen 1 viewShift)

   --    gridselectWorkspace gsConfig S.greedyView)
   -- , ((m .|. shiftMask, xK_g   ), windowPromptGoto greenXPConfig
                                    -- { autoComplete = Just 500000 } )
   , ((m,               xK_g   ), goToSelected gsConfig)
   , ((m .|. shiftMask, xK_g   ),
         gets windowset
            >>= windowSpacesNumTitles
            >>= (\wnts -> gridselectWorkspaceShow
                        gsConfigWorkspace
                        (showWorkspace wnts)
                        greedyView)
            >> currentWorkspaceCommand taskCommands)
   , ((m .|. shiftMask .|. controlMask, xK_g   ),
         gets windowset
            >>= windowSpacesNumTitles
            >>= (\wnts -> gridselectWorkspaceShow
                            gsConfigWorkspace
                            (showWorkspace wnts)
                            (\wsp -> greedyView wsp . shift wsp))
            >> currentWorkspaceCommand taskCommands)
   , ((m,               xK_b   ), bringSelected gsConfig)
   ]

   -- Standard keybindings:
   -- mod-[0..9], Switch to workspace N
   -- mod-shift-[0..9], Move client to workspace N
   ++ (zip (zip (repeat m) [xK_0..xK_9])
        $ map switchNthLastFocused [0..])
   ++ (zip (zip (repeat $ m .|. shiftMask) [xK_0..xK_9])
        $ map shiftNthLastFocused [0..])
   -- mod-control-[0..9] moves window to workspace
   ++ (zip (zip (repeat $ m .|. controlMask) [xK_0..xK_9])
        $ map (\w -> (shiftNthLastFocused w)
                     >> (switchNthLastFocused w))
              [0..9])
   where
      m = modMask conf
      withScreen myscreen f = screenWorkspace myscreen
                              >>= flip whenJust (windows . f)
      viewShift  i   = view i . shift i

gsfont :: String
gsfont =
   "xft:Inconsolata-g:pixelsize=15:antialias=true:hinting=true"

gsConfigWorkspace   :: GSConfig WorkspaceId
gsConfigWorkspace = def { gs_cellwidth = 400 -- 800
                        , gs_cellheight= 30
                        , gs_font      = gsfont
                        }

gsConfig   :: GSConfig Window
gsConfig   = def { gs_cellwidth = 400 -- 800
                        , gs_cellheight= 30
                        , gs_font      = gsfont
                        }

showWorkspace :: [(WorkspaceId,(Int,String))] -> WindowSpace -> String
showWorkspace wnts w =
  -- (showNumberOfWindows . Just $ w)
  show n
    ++ " "
    ++ (marshall scr . xmobarShowTask . tag $ w)
    ++ " "
    -- ++ (headDef "" . words) t
    ++ t
  where scr = unmarshallS . tag $ w
        (n,t) = fromJustDef (0,"") $ Data.List.lookup (tag w) wnts

