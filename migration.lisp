(in-package :migration-user)

(setf *db-connection-parameters*
'("planet_git" "gitui" "oenRTe90u" "localhost"))

(def-query-migration 1 "drop key"
  :execute "DROP TABLE keys")

(def-query-migration 2 "add location column to login"
  :execute "ALTER TABLE login ADD COLUMN location text")
