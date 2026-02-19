import { mkdtempSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";

let counter = 0;

export const mkTempDbUrl = () => {
  const dir = mkdtempSync(join(tmpdir(), "yoga-sqlite-test-"));
  counter++;
  return "file:" + join(dir, `test${counter}.db`);
};
