-- CSpydr neovim plugin

local function setup()
    vim.cmd([[
        autocmd BufRead,BufNewFile *.smll set filetype=smll
        autocmd Syntax smll runtime! syntax/smll.vim
    ]])
end

return {
    setup = setup,
}
