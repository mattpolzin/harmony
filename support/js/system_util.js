// like base library popen but with stderr capture
// as well
function system_util_popen_plus (cmd, m) {
  const mode = support_system_file_parseMode(m)
  if (mode != 'r') {
    process.__lasterr = 'The NodeJS popen FFI only supports opening for reading currently.'
    return null
  }

  const tmp_file1 = require('os').tmpdir() + "/" + require('crypto').randomBytes(15).toString('hex')
  const write_fd = support_system_file_fs.openSync(
    tmp_file1,
    'w'
  )

  const tmp_file2 = require('os').tmpdir() + "/" + require('crypto').randomBytes(15).toString('hex')
  const stderr_fd = support_system_file_fs.openSync(
    tmp_file2,
    'w'
  )

  var io_setting
  switch (mode) {
    case "r":
      io_setting = ['ignore', write_fd, stderr_fd]
      break
    case "w":
    case "a":
      io_setting = [write_fd, 'ignore', stderr_fd]
      break
    default:
      process.__lasterr = 'The popen function cannot be used for reading and writing simultaneously.'
      return null
  }

  const { status, error  } = support_system_file_child_process.spawnSync(
    cmd,
    [],
    { stdio: io_setting, shell: true }
  )

  support_system_file_fs.closeSync(write_fd)
  support_system_file_fs.closeSync(stderr_fd)

  if (error) {
    process.__lasterr = error
    return null
  }

  const read_ptr = support_system_file_openFile(
    tmp_file1,
    'r'
  )
  const stderr_ptr = support_system_file_openFile(
    tmp_file2,
    'r'
  )

  return { stdpipe: read_ptr, stderr: stderr_ptr, exit_code: status }
}

function system_util_get_pipe_fd(popen_plus_ptr) {
  return popen_plus_ptr.stdpipe
}

function system_util_get_err_fd(popen_plus_ptr) {
  return popen_plus_ptr.stderr
}

// like base library popen but with stderr capture
// as well
function system_util_pclose_plus (popen_plus_ptr) {
  const { stdpipe: {fd, name}, stderr: {fd: stderr_fd, name: stderr_name}, exit_code } = popen_plus_ptr
  support_system_file_fs.closeSync(fd)
  support_system_file_removeFile(name)
  support_system_file_fs.closeSync(stderr_fd)
  support_system_file_removeFile(stderr_name)
  return exit_code
}

