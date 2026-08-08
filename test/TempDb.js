import { mkdtempSync } from "fs";
import { tmpdir } from "os";
import { join } from "path";

const testBytesBacking = new Uint8Array([99, 0, 1, 2, 127, 128, 255, 99]);
export const testBytes = testBytesBacking.subarray(1, 7);
export const uint8ArrayValues = (bytes) => Array.from(bytes);
let counter = 0;

export const mkTempDbUrl = () => {
  const dir = mkdtempSync(join(tmpdir(), "yoga-sqlite-test-"));
  counter++;
  return "file:" + join(dir, `test${counter}.db`);
};
