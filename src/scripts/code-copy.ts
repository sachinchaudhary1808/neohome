import { sleep } from "../utils";

document.querySelectorAll(".code-copy")?.forEach((button) => {
  button.addEventListener("click", async () => {
    const text = button.parentElement?.textContent;
    if (typeof text === "string") {
      const type = "text/plain";
      const blob = new Blob([text], { type });
      const data = [new ClipboardItem({ [type]: blob })];
      await navigator.clipboard.write(data);

      button.classList.add("active");

      await sleep(2000);

      button.classList.remove("active");
    }
  });
});
