-- Leader
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Install lazy.nvim
local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    'git','clone','--filter=blob:none',
    'https://github.com/folke/lazy.nvim.git',
    '--branch=stable',
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Plugins
require('lazy').setup({

  {'numToStr/Comment.nvim', opts = {}},
  {'folke/which-key.nvim', opts = {}},
  {'gbprod/nord.nvim'},

  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "sindrets/diffview.nvim",
      "ibhagwan/fzf-lua",
    },
    config = true
  },

  -- Autocompletion
  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",
    },
    config = function()

      local cmp = require("cmp")
      local luasnip = require("luasnip")

      cmp.setup({
        snippet = {
          expand = function(args)
              luasnip.lsp_expand(args.body)
          end,
        },

        mapping = cmp.mapping.preset.insert({
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<CR>"] = cmp.mapping.confirm({ select = true }),
          ["<Tab>"] = cmp.mapping.select_next_item(),
          ["<S-Tab>"] = cmp.mapping.select_prev_item(),
        }),

        sources = cmp.config.sources({
          { name = "nvim_lsp" },
          { name = "buffer" },
          { name = "path" },
        }),
      })
    end
  },

  -- LSP
  {
    "neovim/nvim-lspconfig",
    dependencies = { "hrsh7th/cmp-nvim-lsp" },
    config = function()

      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      -- Keymaps when LSP attaches
      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function(args)
          local opts = {buffer = args.buf}

          vim.keymap.set("n","gd",vim.lsp.buf.definition,opts)
          vim.keymap.set("n","gD",vim.lsp.buf.declaration,opts)
          vim.keymap.set("n","gi",vim.lsp.buf.implementation,opts)
          vim.keymap.set("n","gr",vim.lsp.buf.references,opts)
          vim.keymap.set("n","K",vim.lsp.buf.hover,opts)
          vim.keymap.set("n", "gl", vim.diagnostic.open_float)

          vim.keymap.set("n","<leader>rn",vim.lsp.buf.rename,opts)
          vim.keymap.set("n","<leader>ca",vim.lsp.buf.code_action,opts)
          vim.keymap.set("n", "<leader>e", vim.diagnostic.setloclist)
          vim.keymap.set("n","[d",vim.diagnostic.goto_prev,opts)
          vim.keymap.set("n","]d",vim.diagnostic.goto_next,opts)

          vim.api.nvim_create_autocmd("FileType", {
              pattern = { "qf", "loclist" },
              callback = function()
                vim.keymap.set("n", "q", "<cmd>close<CR>", { buffer = true, silent = true })
              end,
            })
        end,
      })

        vim.diagnostic.config({
          virtual_text = false, -- cleaner, no inline spam
          signs = true,
          underline = true,
          update_in_insert = false,
          severity_sort = true,
        
          float = {
            border = "rounded",
            source = "if_many",
          },
        })

        vim.api.nvim_create_autocmd("CursorHold", {
          callback = function()
            vim.diagnostic.open_float(nil, { focusable = false })
          end,
        })

      -- Server configs
      vim.lsp.config("clangd", {
        capabilities = capabilities
      })

      vim.lsp.config("lua_ls", {
        capabilities = capabilities,
        settings = {
          Lua = {
            diagnostics = {
              globals = {"vim"}
            }
          }
        }
      })

      -- Enable servers
      vim.lsp.enable("clangd")

    end
  },

  {
    'gelguy/wilder.nvim',
    config = function()
      local wilder = require('wilder')

      wilder.setup({ modes = {':','/','?'} })
      wilder.set_option('use_python_remote_plugin', 0)

      wilder.set_option('pipeline',{
        wilder.branch(
          wilder.cmdline_pipeline(),
          wilder.search_pipeline()
        )
      })

      wilder.set_option('renderer',
        wilder.popupmenu_renderer(
          wilder.popupmenu_border_theme({
            highlighter = wilder.basic_highlighter(),
            border = '',
            min_width = '100%',
            min_height = '20%',
            reverse = 0,
          })
        )
      )
    end
  }

},{})

-- Options

vim.o.hlsearch = true
vim.wo.number = true
vim.o.mouse = 'a'
vim.o.clipboard = 'unnamedplus'
vim.o.breakindent = true
vim.o.undofile = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.signcolumn = 'yes'
vim.o.updatetime = 250
vim.o.timeoutlen = 300

-- Tabs
vim.o.tabstop = 4
vim.o.expandtab = true
vim.o.softtabstop = 4
vim.o.shiftwidth = 4

vim.o.completeopt = 'menuone,noselect'
vim.o.termguicolors = true

-- Disable automatic comment continuation
vim.opt.formatoptions:remove({"r","o"})

vim.api.nvim_create_autocmd("FileType",{
  pattern="*",
  callback=function()
    vim.opt_local.formatoptions:remove({"r","o"})
  end
})

-- Keymaps
vim.keymap.set({'n','v'},'<Space>','<Nop>',{silent=true})

vim.keymap.set('n','k',"v:count==0?'gk':'k'",{expr=true,silent=true})
vim.keymap.set('n','j',"v:count==0?'gj':'j'",{expr=true,silent=true})

-- Windows
vim.keymap.set("n","<leader>wv",vim.cmd.vsp,{desc="Split window vertical"})
vim.keymap.set("n","<leader>ws",vim.cmd.sp,{desc="Split window horizontal"})
vim.keymap.set("n","<leader>wq",vim.cmd.q,{desc="Close window"})
vim.keymap.set("n","<leader>wh","<C-w>h",{desc="Window left"})
vim.keymap.set("n","<leader>wj","<C-w>j",{desc="Window down"})
vim.keymap.set("n","<leader>wk","<C-w>k",{desc="Window up"})
vim.keymap.set("n","<leader>wl","<C-w>l",{desc="Window right"})

-- Buffers
vim.keymap.set("n","<leader>bd",vim.cmd.bdelete,{desc="Delete buffer"})
vim.keymap.set("n","<leader>bb",":FzfLua buffers<CR>",{desc="List buffers"})

-- Find
vim.keymap.set("n","<leader>ff",":FzfLua files<CR>", {desc="Find file"})

-- Config
vim.keymap.set("n","<leader>fp",":e ~/.config/nvim/init.lua<CR>",{desc="Open config"})

-- Search
vim.keymap.set("n","<leader>ss",":FzfLua blines<CR>",{desc="Search buffer"})

-- git
vim.keymap.set("n", "<leader>gg", ":Neogit<CR>", {desc="Open Neogit"})

-- Highlight on yank
local highlight_group = vim.api.nvim_create_augroup('YankHighlight',{clear=true})

vim.api.nvim_create_autocmd('TextYankPost',{
  callback=function()
    vim.highlight.on_yank()
  end,
  group=highlight_group,
  pattern='*',
})

-- Compilation
-- Floating terminal runner for Makefile commands

local last_cmd = nil
local function run_cmd(cmd)
  last_cmd = cmd
  local height = math.floor(vim.o.lines * 0.3)
  
  -- 1. Find the terminal window if it exists
  local win = term_buf and vim.fn.bufwinid(term_buf) or -1

  if win ~= -1 and vim.api.nvim_win_is_valid(win) then
    -- If we are NOT in the terminal window, jump to it
    if vim.api.nvim_get_current_win() ~= win then
      vim.api.nvim_set_current_win(win)
    end
  else
    -- If window doesn't exist, create it from the bottom of the WHOLE screen
    vim.cmd("botright " .. height .. "split")
  end

  -- 2. Create the new buffer FIRST before deleting the old one
  -- This prevents the window from closing/resizing when the old buffer vanishes
  local new_buf = vim.api.nvim_create_buf(false, true)
  
  if term_buf and vim.api.nvim_buf_is_valid(term_buf) then
    vim.api.nvim_buf_delete(term_buf, { force = true })
  end
  
  term_buf = new_buf
  vim.api.nvim_win_set_buf(0, term_buf)

  -- 3. Options & Launch
  vim.wo.number = false
  vim.wo.relativenumber = false
  vim.wo.signcolumn = "no"
  vim.wo.winfixheight = true
  
  vim.fn.termopen(cmd)
  vim.cmd("startinsert")
end

vim.keymap.set('t', '<Esc>', [[<C-\><C-n>]], { desc = "Exit terminal mode" })

-- Better navigation to jump OUT of the terminal split to your code
vim.keymap.set('t', '<C-h>', [[<C-\><C-n><C-w>h]])
vim.keymap.set('t', '<C-j>', [[<C-\><C-n><C-w>j]])
vim.keymap.set('t', '<C-k>', [[<C-\><C-n><C-w>k]])
vim.keymap.set('t', '<C-l>', [[<C-\><C-n><C-w>l]])

-- Compile
vim.keymap.set("n", "<leader>cc", function()
  run_cmd("make")
end, { desc = "Compile project" })

vim.keymap.set("n", "<leader>cC", function()
  run_cmd("make clean")
end, { desc = "Clean project" })

-- Compile + Run
vim.keymap.set("n", "<leader>cr", function()
  run_cmd("make run")
end, { desc = "Compile and run" })

-- Run tests
vim.keymap.set("n", "<leader>ct", function()
  run_cmd("make test")
end, { desc = "Run tests" })

-- Repeat last command
vim.keymap.set("n", "<leader>cl", function()
  if last_cmd then
    run_cmd(last_cmd)
  end
end, { desc = "Repeat last build command" })


-- Colorscheme
vim.cmd.colorscheme("nord")
