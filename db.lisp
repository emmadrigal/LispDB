

(defun tableExists (tableName, dataBase)
     ;; Checks if the name of the table has already been used in an existing table, recursively goes through the tables existing in the dataBase
     (cond ((list-lenght database) > 0) 
          (cond ((eq tableName (caaar Database))  t);;Table name already used, dataBase is composed of tables, first entry in table represents its composition, first entry of the composition is the name
                (t (tableExists (cdr Database)))    ;;Checks for the next table in the DB
          )
          (t nil);;dataBase is empty, table doesn't exist
     )
)

(defun addTable (dataBase)
     ())


(defun functSelect (name) 
     (cond ((eq name 'addt)            (print "Se leyó creación de nueva tabla"))
           ((eq name 'addtable)        (print "Se leyó creación de nueva tabla"))
           ((eq name 'addr)            (print "Se leyó creación de nueva referencia"))
           ((eq name 'addReference)    (print "Se leyó creación de nueva referencia"))
           ((eq name 'remr)            (print "Se leyó eliminacion de referencia"))
           ((eq name 'removeReference) (print "Se leyó eliminacion de referencia"))
           ((eq name 'ins)             (print "Se leyó creación de nuevo record"))
           ((eq name 'insert)          (print "Se leyó el update de un record"))
           ((eq name 'up)              (print "Se leyó el update de un record"))
           ((eq name 'update)          (print "Se leyó creación de nueva record"))
           ((eq name 'rr)              (print "Se leyó creación de nuevo record"))
           ((eq name 'remover)         (print "Se leyó creación de nueva record"))
           ((eq name 'dt)              (print "Se leyó creación de nuevo record"))
           ((eq name 'deltable)        (print "Se leyó creación de nueva record"))
           ((eq name 'query)           (print "Se leyó creación de nuevo record"))
           ((eq name 'cproc)           (print "Se leyó creación de nuevo record"))
           ((eq name 'eval)            (print "Se leyó creación de nueva record"))
           (t                          (print "Comando desconocido"))
           )
)

(defun readFromUser (dataBase)
     (functSelect (read))
     (readFromUser dataBase)
)

(readFromUser '())
