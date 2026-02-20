import { execSync } from "child_process";

let containerId = null;

export const startLibSqlServerImpl = async () => {
  containerId = execSync(
    "podman run -d --rm -P ghcr.io/tursodatabase/libsql-server:latest",
    { encoding: "utf-8" }
  ).trim();
  const portLine = execSync(`podman port ${containerId} 8080`, {
    encoding: "utf-8",
  }).trim();
  const port = portLine.split("\n")[0].split(":").pop();
  const url = `http://127.0.0.1:${port}`;
  for (let i = 0; i < 60; i++) {
    try {
      await fetch(url);
      return url;
    } catch (_) {
      await new Promise((r) => setTimeout(r, 500));
    }
  }
  throw new Error("libsql server didn't start within 30s");
};

export const stopLibSqlServerImpl = async () => {
  if (containerId) {
    try {
      execSync(`podman stop ${containerId}`);
    } catch (_) {}
    containerId = null;
  }
};
