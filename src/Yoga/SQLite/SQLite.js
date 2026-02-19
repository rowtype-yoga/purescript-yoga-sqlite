import { createClient } from "@libsql/client";

// Create client connection
export const createClientImpl = (config) => {
  return createClient(config);
};

// Close connection
export const closeImpl = async (client) => {
  client.close();
};

// Execute query with positional args, return full result
export const queryImpl = async (client, sql, args) => {
  const result = await client.execute({ sql, args });
  return {
    rows: result.rows.map(row => ({ ...row })),
    columns: result.columns,
    rowsAffected: result.rowsAffected,
    lastInsertRowid: result.lastInsertRowid != null
      ? Number(result.lastInsertRowid)
      : null
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
    rowsAffected: result.rowsAffected,
    lastInsertRowid: result.lastInsertRowid != null
      ? Number(result.lastInsertRowid)
      : null
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
    rowsAffected: result.rowsAffected,
    lastInsertRowid: result.lastInsertRowid != null
      ? Number(result.lastInsertRowid)
      : null
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
