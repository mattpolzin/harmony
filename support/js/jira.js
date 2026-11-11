
const https = require('https')
const { Buffer } = require('buffer')

const jira_config = (username, token, hostname) => {
  const cfg = new Object()
  cfg.hostname = hostname
  cfg.username = username
  cfg.token = token
  return cfg
}

const jira_list_issues = (config, jql, onSuccess, onFailure) =>
  idris__jira_request(
      config
    , '/rest/api/3/search/jql'
    , 'POST'
    , { jql
      , fields: ['id','key']
      }
    , r => onSuccess(r)()
    , e => onFailure(e)()
  )

const jira_get_me = (config, onSuccess, onFailure) =>
  idris__jira_request(
      config
    , '/rest/api/3/myself'
    , 'GET'
    , {}
    , r => onSuccess(r)()
    , e => onFailure(e)()
  )

const idris__jira_request = (config, path, method, bodyObj, onSuccess, onFailure) => {
  const bodyData = JSON.stringify(bodyObj)

  const {
    hostname,
    username,
    token
  } = config

  const auth = `${username}:${token}`

  const headers = {
    'Accept': 'application/json',
  }

  if (method == 'POST') {
    headers['Content-Length'] = Buffer.byteLength(bodyData)
    headers['Content-Type'] = 'application/json'
  }

  const options = {
    hostname,
    path,
    method,
    headers,
    auth,
    agent: false,
  }

  const req = https.request(options, (res) => {
    res.setEncoding('utf8')

    var respContent = ''
    res.on('data', (chunk) => {
      respContent += chunk.toString()
    })
    res.on('end', () => {
      onSuccess(respContent)
    })
  })

  req.on('error', (e) => {
    onFailure(JSON.stringify(e))
  })

  if (method == 'POST') {
    req.write(bodyData)
  }
  req.end()
}
