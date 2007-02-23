/*

 $Header: /project/cmucl/cvsroot/src/motif/server/functions.h,v 1.3 1997/08/22 20:49:33 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

/* In requests.c */
extern QuitServer();

/* in callbacks.c */
extern TerminateCallback();
extern RXtAddCallback();
extern RXtRemoveCallback();
extern RXmAddProtocolCallback();
extern RXmRemoveProtocolCallback();
extern RReturnTextCallbackDoit();

/* in events.c */
extern RTransportEvent();
extern RXtAddEventHandler();
extern RXtRemoveEventHandler();

/* in widgets.c */
extern RXtAppCreateShell();
extern RXtCreateManagedWidget();
extern RXtCreateWidget();
extern RXtDestroyWidget();
extern RXtRealizeWidget();
extern RXtUnrealizeWidget();
extern RXtMapWidget();
extern RXtUnmapWidget();
extern RXtSetSensitive();
extern RXtCreatePopupShell();
extern RXtPopup();
extern RXtManageChild();
extern RXtUnmanageChild();
extern RXtManageChildren();
extern RXtUnmanageChildren();
extern RXtPopdown();
extern RXtIsManaged();
extern RXtPopupSpringLoaded();
extern RXtIsRealized();
extern RXtWindow();
extern RXtName();
extern RXtIsSensitive();
extern RXtIsApplicationShell();
extern RXtIsComposite();
extern RXtIsConstraint();
extern RXtIsObject();
extern RXtIsOverrideShell();
extern RXtIsRectObj();
extern RXtIsShell();
extern RXtIsTopLevelShell();
extern RXtIsTransientShell();
extern RXtIsVendorShell();
extern RXtIsWMShell();
extern RXtNameToWidget();
extern RXtParent();
extern RXtTranslateCoords();
extern RXmCreateMenuBar();
extern RXmCreateOptionMenu();
extern RXmCreateRadioBox();
extern RXmCreateWarningDialog();
extern RXmCreateBulletinBoardDialog();
extern RXmCreateErrorDialog();
extern RXmCreateFileSelectionDialog();
extern RXmCreateFormDialog();
extern RXmCreateInformationDialog();
extern RXmCreateMessageDialog();
extern RXmCreatePopupMenu();
extern RXmCreatePromptDialog();
extern RXmCreatePulldownMenu();
extern RXmCreateQuestionDialog();
extern RXmCreateScrolledList();
extern RXmCreateScrolledText();
extern RXmCreateSelectionDialog();
extern RXmCreateWorkingDialog();
extern RXCreateFontCursor();

/* In resources.c */
extern RXtSetValues();
extern RXtGetValues();

/* In motif.c */
extern RXmUpdateDisplay();
extern RXmIsMotifWMRunning();
extern RXmCommandAppendValue();
extern RXmCommandError();
extern RXmCommandSetValue();
extern RXmScaleGetValue();
extern RXmScaleSetValue();
extern RXmToggleButtonGetState();
extern RXmToggleButtonSetState();
extern RXmAddTabGroup();
extern RXmRemoveTabGroup();
extern RXmProcessTraversal();
extern RXmMessageBoxGetChild();
extern RXmSelectionBoxGetChild();
extern RXmFileSelectionBoxGetChild();
extern RXmCommandGetChild();
extern RXmScrolledWindowSetAreas();
extern RXmTrackingLocate();
extern RXmMenuPosition();
extern RXmScrollBarGetValues();
extern RXmScrollBarSetValues();

/* In translations.c */
extern RXtParseTranslationTable();
extern RXtAugmentTranslations();
extern RXtOverrideTranslations();
extern RXtUninstallTranslations();
extern RXtParseAcceleratorTable();
extern RXtInstallAccelerators();
extern RXtInstallAllAccelerators();

/* In text.c */
extern RXmTextClearSelection();
extern RXmTextCopy();
extern RXmTextCut();
extern RXmTextGetBaseline();
extern RXmTextGetEditable();
extern RXmTextGetInsertionPosition();
extern RXmTextGetLastPosition();
extern RXmTextGetMaxLength();
extern RXmTextGetSelection();
extern RXmTextGetSelectionPosition();
extern RXmTextGetString();
extern RXmTextGetTopCharacter();
extern RXmTextInsert();
extern RXmTextPaste();
extern RXmTextPosToXY();
extern RXmTextRemove();
extern RXmTextReplace();
extern RXmTextScroll();
extern RXmTextSetAddMode();
extern RXmTextSetEditable();
extern RXmTextSetHighlight();
extern RXmTextSetInsertionPosition();
extern RXmTextSetMaxLength();
extern RXmTextSetSelection();
extern RXmTextSetString();
extern RXmTextSetTopCharacter();
extern RXmTextShowPosition();
extern RXmTextXYToPos();

/* In xmstring.c */
extern RXmFontListAdd();
extern RXmFontListCreate();
extern RXmFontListFree();
extern RXmStringBaseline();
extern RXmStringByteCompare();
extern RXmStringCompare();
extern RXmStringConcat();
extern RXmStringCopy();
extern RXmStringCreate();
extern RXmStringCreateLtoR();
extern RXmStringGetLtoR();
extern RXmStringCreateSimple();
extern RXmStringEmpty();
extern RXmStringExtent();
extern RXmStringFree();
extern RXmStringHasSubstring();
extern RXmStringHeight();
extern RXmStringLength();
extern RXmStringLineCount();
extern RXmStringNConcat();
extern RXmStringNCopy();
extern RXmStringSeparatorCreate();
extern RXmStringWidth();

/* In list.c */
extern RSetItems();
extern RGetItems();
extern RXmListAddItem();
extern RXmListAddItemUnselected();
extern RXmListDeleteItem();
extern RXmListDeletePos();
extern RXmListDeselectAllItems();
extern RXmListDeselectItem();
extern RXmListDeselectPos();
extern RXmListSelectItem();
extern RXmListSelectPos();
extern RXmListSetBottomItem();
extern RXmListSetBottomPos();
extern RXmListSetHorizPos();
extern RXmListSetItem();
extern RXmListSetPos();
extern RXmListAddItems();
extern RXmListDeleteAllItems();
extern RXmListDeleteItems();
extern RXmListDeleteItemsPos();
extern RXmListItemExists();
extern RXmListItemPos();
extern RXmListReplaceItems();
extern RXmListReplaceItemsPos();
extern RXmListSetAddMode();
extern RXmListGetSelectedPos();
