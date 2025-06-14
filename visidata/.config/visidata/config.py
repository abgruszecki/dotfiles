@VisiData.api
def my_ipy():
    with SuspendCurses():
        import IPython
        # We talk to the terminal to switch to an "alternative buffer" (like Vim).
        # This saves the current terminal state and lets us operate like an app.
        # Curses maintains its own alternative buffer and drops back
        # to the regular one when suspended, so we just manually switch
        # to an alternative buffer again.
        # Slight downside: we don't have history, but that's too bad I suppose.
        print('\033[?1049h\033[H', end='')
        print('Alternate buffer!')
        # This should allow accessing the local namespace
        # For more configurability see:
        # https://ipython.readthedocs.io/en/stable/api/generated/IPython.terminal.embed.html#IPython.terminal.embed.InteractiveShellEmbed
        IPython.embed()
        print('\033[?1049l', end='')


import subprocess
@Sheet.api
def my_view_cell(sheet):
    with SuspendCurses():
        subprocess.run(
            ['vim', '-'],
            input=(str(sheet.cursorValue)).encode(),
        )

BaseSheet.addCommand(
    None, # keystrokes
    'my-view-cell', # command name
    'my_view_cell()', # ...execstr
    'view current cell in vim', # description
)


from visidata import rotateRange
_my_addr = None
_my_addr_keycol_names = None
_my_pre_jump_idx = None
_my_pre_jump_sheet = None

@Sheet.api
def my_store_key_address(sheet):
    global _my_addr, _my_addr_keycol_names, _my_pre_jump_idx, _my_pre_jump_sheet
    if not sheet.keyCols:
        vd.error('Current sheet has no key columns!')
        return

    _my_addr = sheet.rowkey(sheet.cursorRow)
    _my_addr_keycol_names = [c.name for c in sheet.keyCols]

@Sheet.api
def my_jump_to_key_address(sheet):
    global _my_addr, _my_addr_keycol_names, _my_pre_jump_idx, _my_pre_jump_sheet
    if not _my_addr:
        vd.error('No address stored yet!')
        return
    if not sheet.keyCols:
        vd.error('Current sheet has no key columns!')
        return

    cur_sheet_keys_num = len(sheet.keyCols)
    cur_sheet_key_names = [c.name for c in sheet.keyCols]
    stored_addr_keys_num = len(_my_addr)
    stored_keycol_names = _my_addr_keycol_names

    if cur_sheet_keys_num != stored_addr_keys_num:
        vd.error(f'Key count mismatch: stored={stored_addr_keys_num} , current={cur_sheet_keys_num}')
        return # ...shouldn't these returns be errors? :return-error:
    if cur_sheet_key_names != stored_keycol_names:
        vd.error(f'Key name mismatch (rename cols?): stored={", ".join(stored_keycol_names)} ; current={", ".join(cur_sheet_key_names)}')
        return # :return-error:

    for idx in rotateRange(len(sheet.rows), sheet.cursorRowIndex):
        r = sheet.rows[idx]
        if sheet.rowkey(r) == _my_addr:
            _my_pre_jump_idx = sheet.cursorRowIndex
            _my_pre_jump_sheet = sheet
            sheet.cursorRowIndex = idx
            return
    vd.error(f'Key address not found (type mismatch?): {_my_addr!r}')

@Sheet.api
def my_revert_jump_to_key_address(sheet):
    global _my_addr, _my_addr_keycol_names, _my_pre_jump_idx, _my_pre_jump_sheet
    if _my_pre_jump_idx is None:
        vd.error('No jump done yet!')
        return
    if _my_pre_jump_sheet is not sheet:
        vd.error(f'Last jump was in a different sheet - {_my_pre_jump_sheet.name}')
        # NOTE ...I guess the current sheet's index can be found in vd.sheets?
        return
    sheet.cursorRowIndex = _my_pre_jump_idx

BaseSheet.addCommand(
    None,
    'my-store-key-address',
    'my_store_key_address()',
    'store current key address',
)

BaseSheet.addCommand(
    None,
    'my-jump-to-key-address',
    'my_jump_to_key_address()',
    'jump to a stored key address',
)

BaseSheet.addCommand(
    None,
    'my-revert-jump-to-key-address',
    'my_revert_jump_to_key_address()',
    'revert the last jump to a stored key address',
)


@Sheet.api # this adds a method to Sheet classes
def my_toggle_width_col(sheet):
    try:
        col = sheet.cursorCol
        col.setWidth(
            8 if col.width != 8 else 20
        )
    except Exception as e:
        vd.fail(str(e))

BaseSheet.addCommand(
    None, # keystrokes
    'my-toggle-width-col', # command name
    'my_toggle_width_col()', # ...execstr
    'are you for real', # description
)

# addGlobals({
#     'my_toggle_width': my_toggle_width
# })

BaseSheet.unbindkey('F2')
BaseSheet.bindkey('F2', 'my-jump-to-key-address')
BaseSheet.bindkey('^F2', 'my-store-key-address')
BaseSheet.bindkey('F3', 'syscopy-cell')
BaseSheet.bindkey('F4', 'my-view-cell')
BaseSheet.bindkey('Alt+_', 'my-toggle-width-col')
#BaseSheet.bindkey('Alt+:', '')
