local langs = {
	"astro",
	"javascript",
	"typescript",
  "javascriptreact",
  "typescriptreact",
  "json",
}

for _, lang in ipairs(langs) do
	vim.api.nvim_create_autocmd("FileType", {
		pattern = lang,
		callback = function()
			vim.opt_local.tabstop = 4
			vim.opt_local.shiftwidth = 4
			vim.opt_local.expandtab = true
		end,
	})
end
