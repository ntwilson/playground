const sql = require('mssql/msnodesqlv8');

(async function () { 
  try {
    let conn = await sql.connect({
      server: 'mssql03.ad.mea.energy',
      database: 'SystemTestSource',
      options: { trustedConnection: true }
    })

    let result = await conn.query('SELECT TOP 10 * FROM KeyedValues')
    console.log(result)
  }
  catch(e) {
    console.log(e)
  }
})()
