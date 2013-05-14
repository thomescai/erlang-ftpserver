
-record(connection_state,
        {
          authenticated_state = unauthenticated,
          user_name,
		  users,
          data_port = undefined,
          pasv_listen = undefined,
          ip_address = undefined,
          rnfr = undefined,
          module,
          module_state,
		  root_dir = "",
		  current_dir = "/",
          ssl_allowed = false,
          ssl_cert = undefined,
          ssl_key = undefined,
          ssl_ca_cert = undefined,
          protection_mode = clear,
          pb_size = 0,
          control_socket = undefined,
          ssl_socket = undefined
         }).

-record(file_info,
        {
		  size,
          type, % device | directory | other | regular | symlink
          access, % read | write | read_write | none
		  atime,
		  mtime,
		  ctime,
          mode,
          links,
          major_device,
          minor_device,
          inode,
          uid,
          gid
         }).



-define (DEFAULT_PORT, 21).
-define (DEFAULT_LOCAL_HOST, '127.0.0.1').

-define(FILE_PRE_READ_TOM(X),X*1024*1024).
-define (DEFAULT_ROOT_DIR, "~").

