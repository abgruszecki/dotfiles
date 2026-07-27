if vim.fn.has("nvim-0.9") ~= 1 then
  vim.notify("fzf-lua requires Neovim >= 0.9", vim.log.levels.WARN)
  return
end

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
local uv = vim.uv or vim.loop

if not uv.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  {
    "ibhagwan/fzf-lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {},
    keys = {
      {
        "<C-x><C-f>",
        function()
          require("fzf-lua").complete_path()
        end,
        mode = { "n", "v", "i" },
        desc = "Fuzzy complete path",
      },
    },
  },
}, {
  change_detection = {
    notify = false,
  },
  performance = {
    reset_packpath = false,
    rtp = {
      reset = false,
    },
  },
})
