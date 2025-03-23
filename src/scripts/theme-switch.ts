const toggle = document.getElementById("themeToggle");

if (toggle !== null) {
  toggle.onclick = () => {
    document.documentElement.classList.toggle("dark");

    const isDark = document.documentElement.classList.contains("dark");
    localStorage.setItem("theme", isDark ? "dark" : "light");

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (window as any).loadTheme(document);
  };
}
