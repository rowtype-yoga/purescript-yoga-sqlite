import Database from 'better-sqlite3';

// Open a database connection
export const openImpl = (path) => {
  const db = new Database(path);
  // Enable foreign keys by default
  db.pragma('foreign_keys = ON');
  return db;
};

// Close a database connection
export const closeImpl = (db) => {
  db.close();
};

// Execute a statement (INSERT/UPDATE/DELETE)
export const execImpl = (sql, params, db) => {
  const stmt = db.prepare(sql);
  stmt.run(...params);
};

// Query for multiple rows
export const queryImpl = (sql, params, db) => {
  const stmt = db.prepare(sql);
  const rows = stmt.all(...params);
  // Convert rows to arrays of values
  return rows.map(row => Object.values(row));
};

// Query for a single row
export const queryOneImpl = (sql, params, db) => {
  const stmt = db.prepare(sql);
  const row = stmt.get(...params);
  if (!row) return null;
  // Convert row to array of values
  return Object.values(row);
};

// Get last inserted row ID
export const lastInsertRowIdImpl = (db) => {
  const result = db.prepare('SELECT last_insert_rowid() as id').get();
  return result.id;
};

// Begin a transaction
export const beginTransactionImpl = (db) => {
  return db.transaction(() => { });
};

// Commit a transaction (no-op with better-sqlite3, handled automatically)
export const commitImpl = (_txn) => {
  // better-sqlite3 auto-commits when transaction function completes
};

// Rollback a transaction (no-op with better-sqlite3, handled by throwing)
export const rollbackImpl = (_txn) => {
  // better-sqlite3 auto-rolls back when transaction function throws
};
