const sql = require('mssql/msnodesqlv8');

exports.connectImpl = function (server, database) { 
  return sql.connect({
    server: server,
    database: database,
    options: { trustedConnection: true }
  });
}

exports.queryImpl = function(conn, queryString) {
  return conn.query(queryString)
    .then(function (result) { 
      return Promise.resolve(result.recordset);
    });
}

exports.closeImpl = function(conn) {
  return conn.close();
}
