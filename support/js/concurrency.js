
const { exec: idris__concurrency_exec } = require('child_process')

// Create a "Future" by forking to another process executing harmony with
// the given argString (space separated CLI arguments to harmony).
//
// The result is a Javascript Promise with the String value of stdout from the forked
// process. This type of promise is opaquely wrapped in the Idris "Future" type.
const concurrency_future = argString => {
  return new Promise(
    (onSuccess, onFailure) => {
      idris__concurrency_exec(`harmony ${argString}`,(error, stdout, stderr) => {
	if (error) { onFailure(stderr) }
	else { onSuccess(stdout) }
      })
    }
  )
}

const idris__concurrency_stringyArray = v =>
  (Array.isArray(v) ? JSON.stringify(v.map(JSON.parse)) : v)

// Await the completion of the given "Future" (Javascript Promise).
//
// The assumption is that the result of the promise is JSON. If the promise
// was a singular Future, it is passed through, but if the result is the
// the Promise.all of multiple futures, each result is parsed and then the
// entire array of results is stringified.
const concurrency_await_stringify = (promise, onSuccess, onFailure) =>
  promise.then(
    res => onSuccess(idris__concurrency_stringyArray(res))(),
    err => onFailure(err)()
  )

