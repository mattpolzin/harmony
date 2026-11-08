
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
    config,
    { jql
    , fields: ['id','key']
    }
    , r => onSuccess(r)()
    , e => onFailure(e)()
  )

const idris__jira_request = (config, postObj, onSuccess, onFailure) => {
  const postData = JSON.stringify(postObj)

  const {
    hostname,
    username,
    token
  } = config

  const options = {
    hostname,
    auth: `${username}:${token}`,
    path: '/rest/api/3/search/jql',
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Content-Length': Buffer.byteLength(postData),
    },
    agent: false,
  };

  const req = https.request(options, (res) => {
    res.setEncoding('utf8');

    var respContent = ''
    res.on('data', (chunk) => {
      respContent += chunk.toString()
    }) ;
    res.on('end', () => {
      onSuccess(respContent)
    });
  });

  req.on('error', (e) => {
    onFailure(JSON.stringify(e))
  });

  req.write(postData);
  req.end();
}
