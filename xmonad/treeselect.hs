-----------------
-- Tree Select --
-----------------

treeSelectAction :: TS.TSConfig (X ()) -> X ()
treeSelectAction config = TS.treeselectAction config
  [ Node (TS.TSNode "Applications" "a list of programs I use often" (return ())) [ Node (TS.TSNode name desc (spawn app)) [] | (name, app, desc) <- myProgram]
  , Node (TS.TSNode "Configurations" "config files that edit often" (return ())) [ Node (TS.TSNode name desc (spawn app)) [] | (name, app, desc) <- myConfigs]
  , Node (TS.TSNode "Note Directories" "notes directories list"     (return ())) [ Node (TS.TSNode name desc (spawn app)) [] | (name, app, desc) <- myNoteDir]
  ]

myTreeNavigation = M.fromList
  [ ((0, xK_Escape),             TS.cancel)
  , ((0, xK_q),                  TS.cancel)
  , ((controlMask, xK_g),        TS.cancel)
  , ((0, xK_m),                  TS.select)
  , ((0, xK_Return),             TS.select)
  , ((0, xK_space),              TS.select)
  , ((0, xK_Up),                 TS.movePrev)
  , ((0, xK_Down),               TS.moveNext)
  , ((0, xK_Left),               TS.moveParent)
  , ((0, xK_Right),              TS.moveChild)
    -- vim 
  , ((0, xK_k),                  TS.movePrev)
  , ((0, xK_j),                  TS.moveNext)
  , ((0, xK_h),                  TS.moveParent)
  , ((0, xK_l),                  TS.moveChild)
    -- emacs
  , ((0, xK_n),                  TS.moveNext)
  , ((0, xK_b),                  TS.moveParent)
  , ((0, xK_f),                  TS.moveChild)
  , ((0, xK_p),                  TS.movePrev)
  , ((controlMask, xK_n),        TS.moveNext)
  , ((controlMask, xK_p),        TS.movePrev)
  , ((controlMask, xK_f),        TS.moveChild)
  , ((controlMask, xK_b),        TS.moveParent)
  , ((controlMask, xK_m),        TS.select)
  ]

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig  = TS.TSConfig
  { TS.ts_hidechildren = True
  , TS.ts_background   = draculaBackground
  , TS.ts_font         = "xft:Ubuntu Mono-13"
  , TS.ts_node         = (draculaForeground, draculaSelection)
  , TS.ts_nodealt      = (draculaForeground, draculaSelection)
  , TS.ts_highlight    = (draculaForeground, draculaComment)
  , TS.ts_extra        = draculaComment
  , TS.ts_node_width   = 200
  , TS.ts_node_height  = 30
  , TS.ts_originX      = 0
  , TS.ts_originY      = 0
  , TS.ts_indent       = 80
  , TS.ts_navigate     = myTreeNavigation}

-- Usage: 
--  ("M-S-p" , treeSelectAction tsDefaultConfig)
