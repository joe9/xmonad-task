
{-# OPTIONS_GHC -Wall #-}
module XMonad.Config.TaskConfig
   ( taskConfig
   , startupTasks
   , gridselectWorkspaceShowAndGoto
   , xmobarShowTaskWithNumberOfWindowsAndFocussedTitle
   , windowSpacesNumTitles
   ) where

-- system imports
import           Control.Monad                    (liftM2)
import           Data.List                        (lookup)
import           Data.Map                         (Map, fromList,
                                                   union)
import qualified Data.Map                         as M (Map, fromList,
                                                        lookup)
import           Safe                             (fromJustDef,
                                                   headDef, readMay)
import           System.FilePath                  (dropTrailingPathSeparator,
                                                   takeBaseName)

-- xmonad core
import           XMonad                           hiding (focus)
import           XMonad.StackSet                  hiding (workspaces)
import qualified XMonad.StackSet                  as S (workspaces)

-- xmonad contrib
import           XMonad.Actions.Commands          (defaultCommands)
import           XMonad.Actions.CycleRecentWS     (cycleWindowSets)
import           XMonad.Actions.CycleWindows      (rotUp)
import           XMonad.Actions.GridSelect        (GSConfig (gs_cellheight, gs_cellwidth, gs_font),
                                                   bringSelected,
                                                   goToSelected, gridselectWorkspaceShow)
import           XMonad.Config.Desktop            (desktopConfig)
import           XMonad.Hooks.DynamicBars         (dynStatusBarEventHook,
                                                   multiPP)
import           XMonad.Hooks.DynamicLog          (xmobarColor)
import           XMonad.Hooks.ManageDocks         (AvoidStruts, Direction2D (D, L, R, U))
import           XMonad.Hooks.ServerMode          (serverModeEventHookF)
import qualified XMonad.Layout.BoringWindows      as B (focusMaster)
import           XMonad.Layout.IndependentScreens (PhysicalWorkspace,
                                                   VirtualWorkspace,
                                                   unmarshall,
                                                   unmarshallW)
import           XMonad.Layout.LayoutModifier     (ModifiedLayout)
import           XMonad.Layout.WindowNavigation   (Navigate (Go))
import           XMonad.Util.NamedWindows         (getName)

-- local imports
import           XMonad.Actions.Task
import           XMonad.Config.TaskActionConfig   (taskActions)
import           XMonad.Config.XmobarConfig       (barCreator,
                                                   barDestroyer, pp)
import           XMonad.Hooks.TaskCommands        (serverModeTaskEventHookCmd')
-- import           XMonad.Util.DTrace               (dtrace)

taskConfig :: XConfig
                (XMonad.Layout.LayoutModifier.ModifiedLayout
                   XMonad.Hooks.ManageDocks.AvoidStruts
                   (Choose Tall (Choose (Mirror Tall) Full)))
taskConfig =
  desktopConfig
    { handleEventHook       =
         serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
            <+> serverModeTaskEventHookCmd' taskActions defaultCommands
            <+> dynStatusBarEventHook
                 barCreator
                 barDestroyer
    -- 1 = startId
    -- 2 = number of screens
    , workspaces  = startupTaskWorkspaces 1 2 startupTasks
    -- can use a custom function instead of
    --   xmobarShowTaskWithNumberOfWindowsAndFocussedTitle
    , logHook     =
      tasksPP windowSpacesNumTitles taskActions pp
        >>= (\p -> multiPP p p)
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
startupTasks = tasks
tasks =
  (  concat
        . replicate 3
        . map (\s -> Task "terminal" (S s) "/home/j/" 0)
        $ [0..1])
    ++   concatMap f   [ "/home/j/etc/zsh"
                      , "/home/j/etc/X11"
                      , "/home/j/etc/emacs/emacs.d"
                      ]
    ++   map ff1   [ "/home/j/Desktop/"
                        , "/home/j/etc/emacs/emacs.d"
                        , "/home/j/"
                        , "/home/j/"
                        , "/var/log"
                        ]
    ++   map ff0 [ "/home/j/"
                , "/home/j/etc/xmonad"
                ]
    ++   [  t  "None"            1  "/tmp"
        ,  t  "xmonad-compile"  1  "/home/j/etc/xmonad"
        ,  t  "mplayer"         1  "/tmp/"
        ]
  where
      t n s d = Task n (S s) d 0
      f d = map (\s -> Task "terminal" (S s) d 0) [0..1]
      ff1 d = Task "terminal" (S 1) d 0
      ff0 d = Task "terminal" (S 0) d 0

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
         g greedyView)
   , ((m .|. shiftMask .|. controlMask, xK_g   ),
         g (\wsp -> greedyView wsp . shift wsp))
   , ((m,               xK_b   ), bringSelected gsConfig)
   ]
   -- Standard keybindings:
   -- mod-[0..9], Switch to workspace N
   -- mod-shift-[0..9], Move client to workspace N
   ++ zip (zip (repeat m) [xK_0..xK_9])
        (map switchNthLastFocused [0..])
   ++ zip (zip (repeat $ m .|. shiftMask) [xK_0..xK_9])
         (map shiftNthLastFocused [0..])
   -- mod-control-[0..9] moves window to workspace
   ++ zip (zip (repeat $ m .|. controlMask) [xK_0..xK_9])
         (map (\w -> shiftNthLastFocused w >> switchNthLastFocused w)
              [0..9])
   where
      m = modMask conf
      withScreen myscreen f = screenWorkspace myscreen
                              >>= flip whenJust (windows . f)
      viewShift  i   = view i . shift i
      g = gridselectWorkspaceShowAndGoto
            windowSpacesNumTitles
            taskActions

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

gridselectWorkspaceShowAndGoto ::
  (WindowSet -> X (M.Map WorkspaceId b))
    -> TaskActions a b
    -> (WorkspaceId -> WindowSet -> WindowSet)
    -> X ()
gridselectWorkspaceShowAndGoto f taskactions h =
    gets windowset
       >>= f
       >>= (\wsInfo -> gridselectWorkspaceShow
                        gsConfigWorkspace
                        (gridselectShowWindowSpace taskactions wsInfo)
                        h)
       >> currentWorkspaceAction taskactions

gridselectShowWindowSpace :: TaskActions a b
                                -> M.Map WorkspaceId b
                                -> WindowSpace
                                -> String
gridselectShowWindowSpace tas bs ws =
  (\f -> j f (M.lookup (tag ws) bs) (toTask ws) ws)
                . taGridSelectShow
                . fromJustDef nullTaskAction
                . flip M.lookup tas
                . tAction
                . toTask
                $ ws
  where j _  (Nothing) _ = tag
        j f (Just b)   t = f b t
        toTask = workspaceIdToTask . tag

windowSpacesNumTitles :: WindowSet -> X (M.Map WorkspaceId (Int,String))
windowSpacesNumTitles s =
  (mapM windowSpaceNumTitle . S.workspaces $ s)
     >>= (return . M.fromList)

windowSpaceNumTitle :: WindowSpace -> X (WorkspaceId,(Int,String))
windowSpaceNumTitle ws = do
  -- io . dtrace $ "windowSpaceNumTitle: workspaceId " ++ (tag ws)
  wTitle <- maybe (return "") (fmap show . getName . focus) . stack $ ws
  return (tag ws,(numOfWindows ws, wTitle))

numOfWindows :: WindowSpace -> Int
numOfWindows = length . integrate' . stack

xmobarShowTaskWithNumberOfWindowsAndFocussedTitle ::
  [(WorkspaceId,(Int,String))]
        -> PhysicalWorkspace
        -> String
xmobarShowTaskWithNumberOfWindowsAndFocussedTitle wnts wsid =
  xmobarColor "white" "" (xmobarShowTask wsid)
   ++ "-"
   ++ show n
   ++ "-"
   ++ (headDef "" . words) t
   where (n,t) = fromJustDef (0,"") $ Data.List.lookup wsid wnts

xmobarShowTask :: PhysicalWorkspace -> String
xmobarShowTask w =
  showTask w . (readMay :: String -> Maybe Task) . unmarshallW $ w

showTask :: VirtualWorkspace -> Maybe Task -> String
showTask w (Nothing) = w
showTask _ (Just t) =
  takeBaseName . dropTrailingPathSeparator . tDir $ t
