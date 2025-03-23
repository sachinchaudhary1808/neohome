document.getElementById("themeToggle")?.addEventListener("click", () => {
  const theme = localStorage.getItem("theme") as "dark" | "light" | null;

  const newTheme = (() => {
    if (theme === null) {
      if (
        window.matchMedia &&
        window.matchMedia("(prefers-color-scheme: dark)").matches
      ) {
        // Prefers dark, switch to light
        return "light";
      } else {
        return "dark";
      }
    } else if (theme === "light") {
      return "dark";
    } else {
      return "light";
    }
  })();

  localStorage.setItem("theme", newTheme);

  console.log("Theme set to =>", newTheme);

  // @ts-ignore
  globalThis.loadTheme();
});
