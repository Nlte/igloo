-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Install package manager
--    https://github.com/folke/lazy.nvim
--    `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system {
    'git',
    'clone',
    '--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable', -- latest stable release
    lazypath,
  }
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({

  { 'folke/which-key.nvim', opts = {} },
  { 'gbprod/nord.nvim' },
  { "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "sindrets/diffview.nvim",
      "ibhagwan/fzf-lua",
    },
    config = true
  },
  {
    'gelguy/wilder.nvim',
    config = function()
      local wilder = require('wilder')
      wilder.setup({modes = {':', '/', '?'}})
      wilder.set_option('use_python_remote_plugin', 0)

      wilder.set_option('pipeline', {
        wilder.branch(
          wilder.cmdline_pipeline(),
          wilder.search_pipeline()
        ),
      })

      wilder.set_option('renderer', wilder.popupmenu_renderer(
      wilder.popupmenu_border_theme({
        highlighter = wilder.basic_highlighter(),
        border = '',
        min_width = '100%', -- minimum height of the popupmenu, can also be a number
        min_height = '20%', -- to set a fixed height, set max_height to the same value
        reverse = 0,        -- if 1, shows the candidates from bottom to top
      })
    ))

    end,
  }

}, {})

-- [[ Setting options ]]
-- See `:help vim.o`
-- NOTE: You can change these options as you wish!

-- Set highlight on search
vim.o.hlsearch = true

-- Make line numbers default
vim.wo.number = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Keep signcolumn on by default
vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true

-- [[ Basic Keymaps ]]

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Window
vim.keymap.set("n", "<leader>wv", vim.cmd.vsp, { desc = "Split window vertical" })
vim.keymap.set("n", "<leader>ws", vim.cmd.sp, { desc = "Split window horizontal" })
vim.keymap.set("n", "<leader>wq", vim.cmd.q, { desc = "Close window" })
vim.keymap.set("n", "<leader>wh", "<C-w>h", { desc = "Window left" })
vim.keymap.set("n", "<leader>wj", "<C-w>j", { desc = "Window down" })
vim.keymap.set("n", "<leader>wk", "<C-w>k", { desc = "Window up" })
vim.keymap.set("n", "<leader>wl", "<C-w>l", { desc = "Window right" })

-- Buffers
vim.keymap.set("n", "<leader>bd", vim.cmd.bdelete, { desc = "Delete buffer" } )
vim.keymap.set("n", "<leader>bb", ":FzfLua buffers<CR>", { desc = "List buffers" } )

-- Find
vim.keymap.set("n", "<leader>ff", ":FzfLua files<CR>", { desc = "Find file" } )

-- Config
vim.keymap.set("n", "<leader>fp", ":e ~/.config/nvim/init.lua<CR>", { desc = "Open config" } )

-- Search
vim.keymap.set("n", "<leader>ss", ":FzfLua blines<CR>", { desc = "Search buffer" } )


-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = '*',
})

-- [[ Colors ]]
-- 
vim.cmd.colorscheme("nord")
-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
--
