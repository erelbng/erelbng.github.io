document.addEventListener("DOMContentLoaded", function() {
  const btn = document.createElement("button");
  btn.className = "theme-toggle";
  btn.innerText = "🌙 Dark";
  document.body.appendChild(btn);

  // apply system preference at load
  if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
    document.documentElement.setAttribute("data-theme", "dark");
    btn.innerText = "☀️ Light";
  }

  btn.addEventListener("click", () => {
    if (document.documentElement.getAttribute("data-theme") === "dark") {
      document.documentElement.removeAttribute("data-theme");
      btn.innerText = "🌙 Dark";
    } else {
      document.documentElement.setAttribute("data-theme", "dark");
      btn.innerText = "☀️ Light";
    }
  });
});
