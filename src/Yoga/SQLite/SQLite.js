import { createClient } from "@libsql/client";

// Create client connection
export const createClientImpl = (config) => {
  return createClient(config);
};

// Close connection (sync in JS API)
export const closeImpl = (client) => { client.close(); };

// Connection properties
export const closedImpl = (client) => client.closed;
export const protocolImpl = (client) => client.protocol;

// Execute query with positional args, return full result
export const queryImpl = async (client, sql, args) => {
  const result = await client.execute({ sql, args });
  return {
    rows: result.rows.map(row => ({ ...row })),
    columns: result.columns,
    columnTypes: result.columnTypes ?? [],
    rowsAffected: result.rowsAffected,
    lastInsertRowid: result.lastInsertRowid ?? null
  };
};

// Execute query, return only first row or null
export const queryOneImpl = async (client, sql, args) => {
  const result = await client.execute({ sql, args });
  if (result.rows.length === 0) return null;
  return { ...result.rows[0] };
};

// Execute statement, return rows affected count
export const executeImpl = async (client, sql, args) => {
  const result = await client.execute({ sql, args });
  return result.rowsAffected;
};

// Execute simple statement (no args)
export const querySimpleImpl = async (client, sql) => {
  const result = await client.execute(sql);
  return {
    rows: result.rows.map(row => ({ ...row })),
    columns: result.columns,
    columnTypes: result.columnTypes ?? [],
    rowsAffected: result.rowsAffected,
    lastInsertRowid: result.lastInsertRowid ?? null
  };
};

export const executeSimpleImpl = async (client, sql) => {
  const result = await client.execute(sql);
  return result.rowsAffected;
};

// Transaction operations
export const beginImpl = async (client) => {
  return await client.transaction("write");
};

export const beginWithModeImpl = async (client, mode) => {
  return await client.transaction(mode);
};

export const commitImpl = async (tx) => {
  await tx.commit();
};

export const rollbackImpl = async (tx) => {
  await tx.rollback();
};

// Query within transaction
export const txQueryImpl = async (tx, sql, args) => {
  const result = await tx.execute({ sql, args });
  return {
    rows: result.rows.map(row => ({ ...row })),
    columns: result.columns,
    columnTypes: result.columnTypes ?? [],
    rowsAffected: result.rowsAffected,
    lastInsertRowid: result.lastInsertRowid ?? null
  };
};

export const txQueryOneImpl = async (tx, sql, args) => {
  const result = await tx.execute({ sql, args });
  if (result.rows.length === 0) return null;
  return { ...result.rows[0] };
};

export const txExecuteImpl = async (tx, sql, args) => {
  const result = await tx.execute({ sql, args });
  return result.rowsAffected;
};

export const txCloseImpl = (tx) => { tx.close(); };
export const txClosedImpl = (tx) => tx.closed;

export const txBatchImpl = async (tx, stmts) => {
  const results = await tx.batch(stmts.map(s => ({ sql: s.sql, args: s.args })));
  return results.map(r => ({
    rows: r.rows.map(row => ({ ...row })),
    columns: r.columns,
    columnTypes: r.columnTypes ?? [],
    rowsAffected: r.rowsAffected,
    lastInsertRowid: r.lastInsertRowid ?? null
  }));
};

export const txExecuteMultipleImpl = async (tx, sql) => {
  await tx.executeMultiple(sql);
};

// Batch: execute multiple statements atomically
export const batchImpl = async (client, stmts, mode) => {
  const results = await client.batch(stmts.map(s => ({ sql: s.sql, args: s.args })), mode);
  return results.map(r => ({
    rows: r.rows.map(row => ({ ...row })),
    columns: r.columns,
    columnTypes: r.columnTypes ?? [],
    rowsAffected: r.rowsAffected,
    lastInsertRowid: r.lastInsertRowid ?? null
  }));
};

// Sync: replicate from remote
export const syncImpl = async (client) => {
  const result = await client.sync();
  return result ?? null;
};

// Execute multiple statements (semicolon-separated SQL)
export const executeMultipleImpl = async (client, sql) => {
  await client.executeMultiple(sql);
};

// Migrate: run migration statements
export const migrateImpl = async (client, stmts) => {
  const results = await client.migrate(stmts.map(s => ({ sql: s.sql, args: s.args })));
  return results.map(r => ({
    rows: r.rows.map(row => ({ ...row })),
    columns: r.columns,
    columnTypes: r.columnTypes ?? [],
    rowsAffected: r.rowsAffected,
    lastInsertRowid: r.lastInsertRowid ?? null
  }));
};

// Ping: check connection health
export const pingImpl = async (client) => {
  try {
    await client.execute("SELECT 1");
    return true;
  } catch (_err) {
    return false;
  }
};

// Convert DateTime (JSDate) to ISO string for SQLite TEXT storage
export const dateTimeToStringImpl = (jsDate) => jsDate.toISOString();

// F32 vector <-> Array conversion for Turso vector columns
export const f32VectorFromArrayImpl = (arr) => new Float32Array(arr).buffer;
export const f32VectorToArrayImpl = (buf) => Array.from(new Float32Array(buf));

// F64 vector <-> Array conversion for Turso vector columns
export const f64VectorFromArrayImpl = (arr) => new Float64Array(arr).buffer;
export const f64VectorToArrayImpl = (buf) => Array.from(new Float64Array(buf));
