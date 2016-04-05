(defun isKeyAvaliable (privateKey Table)
;; Checks if a private key has already been used inside a table
     (cond ((> (list-length Table) 0)   
            (cond ((string-equal privateKey (caar Table))
                  nil)
            (t (isKeyAvaliable privateKey (cdr Table)))))
     (t t)));;Table is empty, key is available
     
(defun returnRecord (privateKey Table)
;;Returns a specific record, existance of the record has already been proved
      (cond ((string-equal privateKey (caar Table))  (car Table));;Since the private key is unique, a match is the record being looked for
            (t (returnRecord privateKey (cdr Table)))));;Checks the next record in the table

(defun returnTable (tableName dataBase)
;;Returns a specific table, existance of the table has already been proved
      (cond ((string-equal tableName (caaar Database))   (car Database));;Table name already used, dataBase is composed of tables, first entry in table represents its composition, first entry of the composition is the name
            (t (returnTable tableName (cdr Database)))))  ;;Checks for the next table in the DB;;dataBase is empty, table doesn't exist

(defun columnExists (columnName TableTitle)
;;Recieves a the title of a table and the name of column that whishes to be referenced
      (cond ((> (list-length TableTitle) 0)
            (cond ((string-equal (car TableTitle) columnName) t)
                  (t (columnExists columnName (cdr TableTitle)))))
      (t nil)))

(defun referenceError (error dataBase)
    (print error)
    (cond (t dataBase)))

(defun referenceAdder (command dataBase)
;;Based on the commands adds a reference to the dataBase
      (cond ((eq (list-length command) 3)
                    (cond ((and (tableExists (car command) (car dataBase)) (tableExists (caddr command) (car dataBase)))
                                (cond ((< (list-length (returnTable  (car command) (car dataBase))) 2) 
                                            (cond ((columnExists (cadr command) (car (returnTable  (car command) (car dataBase))))
                                                        (append  (list (car dataBase))  (append (cadr Database) (list (list command))) (cddr Database)) )
                                                  (t  (referenceError "The requested column doesn't exists" dataBase)) ))  
                                      (t  (referenceError "The table already has records inserted in it" dataBase) )))
                          (t (referenceError "Requested table doesn't exist" dataBase))))
            (t (referenceError "Incorrect number of arguments" dataBase))))

(defun deleteReference (command oldReferenceTable newReferenceTable)
;;Deletes an entry from the reference table
      (cond ((not (eq 0 (list-length oldReferenceTable))) ;;Checks if all references have been checked
                    (cond ((and (string-equal (car command) (caar oldReferenceTable)) (string-equal (cadr command) (cadar oldReferenceTable)) (string-equal (caddr command) (caddar oldReferenceTable)))
                              (list (append  newReferenceTable (cdr oldReferenceTable))))
                          (t  (deleteReference command (cdr oldReferenceTable) (cons (car oldReferenceTable) newReferenceTable)))))
        (t (list newReferenceTable))))

(defun referenceDeleter (command dataBase)
;;Checks for validity in the recieved command and deletes it
      (cond ((eq (list-length command) 3)
                  (append (list (car dataBase)) (deleteReference command (cadr dataBase) '())  (cddr Database)))
            (t (referenceError "Incorrect number of arguments" dataBase))))

(defun isPKSafe (modifications privateKey)
;;When a record is being modified, it verifies that the privateKey isnt being modified, privateKey referes to the generic name asigned
      (cond ((> (list-length modifications) 0)
            (cond ((string-equal (car modifications) privateKey) nil)
                  (t (isPKSafe (cddr modifications) privateKey)) ))
      (t t)))

(defun changeOneEntry (tableTitle oldRecord modifications newRecord)
;;Goes through the table and changes the specific one required, can make one modification
      (cond ( (> (list-length tableTitle) 0)
            (cond ((string-equal (car tableTitle) (car modifications))
                  (append newRecord (list (cadr modifications)) (cdr oldRecord) ))
            (t (changeOneEntry (cdr tableTitle) (cdr oldRecord) modifications (append newRecord (list (car oldRecord)))))))
      (t newRecord)))

(defun editRecord (modifications record tableTitle)
;;Applies the changeOneEntry to all the requested modifications
      (cond ( (> (list-length modifications) 0)
                  (editRecord  (cddr modifications) (changeOneEntry tableTitle record modifications '())  tableTitle))
            (t record)))

(defun modifyRecord (command Table)
;;Modifies an entry making the required checks, returns the modified table
      (cond ((not (isKeyAvaliable (car command) Table))
            (cond ((evenp (list-length (cdr command)))
                      (cond ((isPKSafe (cdr command) (caar Table)) 
                           (append  (recordDelete (car command) Table '()) (list (editRecord (cdr command) (returnRecord (car command) Table)  (cdar Table))) ))
                      (t (progn (print "Invalid command id being modified") Table))))
                  (t (progn (print "Invalid number of arguments") Table))))
      (t (progn (print "Record doesn't exist") Table))))

(defun recordModifier (command dataBase)
;;modifies records with information based on the information recieved from the user
      (cond ((tableExists (car command) (car dataBase));;Checks existance of the table
            ;;Appends the dataBase minus the modified table and the modified table with the new record
                  (append 
                       (list (append (append (list (modifyRecord (cdr command) (returnTable (car command) (car dataBase))))
                                            (tableDelete (car command) (car dataBase) '()))))
                                      (cdr dataBase)))
            (t dataBase)));;If the table doesn't exist no action is executed

(defun appendNewRecord (command Table)
;;Recieves a command to add a new record and a command to add it to the table
     (cond ((isKeyAvaliable (car command) Table);;Checks availability of the pk
                  (append Table (list command)))
            (t Table)));;If the key isn't available don't modify the record

(defun addRecordError (error dataBase)
    (print error)
    (cond (t dataBase)))

(defun foreignKeyExists (foreignKey foreignTable tableTable)
;;Checks if a value exists as a private key in another table
      (cond ( (not (isKeyAvaliable foreignKey (returnTable foreignTable tableTable)))
                  t);;Foreign key exists
            (t nil)));;Foreign key isn't available

(defun referencesFromTable (tableName referenceTable references)
;;Returns a list with all the references that come from this table
      (cond ((not (eq 0 (list-length referenceTable)))
                  (cond ((string-equal tableName (caar referenceTable))
                              (referencesFromTable tableName (cdr referenceTable) (append references (list(car referenceTable)))))
                        (t (referencesFromTable  tableName  (cdr referenceTable) references))))
            (t references)))

(defun hasForeignKey (tableName referenceTable)
;;Checks if any of the columns is a reference to an foreign table
      (cond ((not (eq 0 (list-length referenceTable)));;Have all references been checked
                (cond ((string-equal tableName (caar referenceTable)) ;; Is the table referenced in the first reference
                            t)
                      (t  (hasForeignKey tableName (cdr referenceTable)))));;Repeat for the next reference
            (t nil)));;Table hasn't been referenced

(defun checkForeignKeys (references command tableTable )
;;Checks if all of the foreignKeys in the command are valid
      (cond ((eq 0 (list-length references))
                t)
            (t (cond ((foreignKeyExists (nth
                                           (columnPosition (car (returnTable (car command) tableTable)) (cadar references) 0) command)
                                           (caddar references)
                                           tableTable)
                                       (checkForeignKeys (cdr references) command tableTable))
                              (t  nil)))))

(defun paramPos (paramList param pos)
;;Returns the position of a parameter according to the data sent by the user
      (cond ((string-equal (car paramList) param)
                pos)
            (t (paramPos (cdr paramList) param (+ 1 pos)))))

(defun paramPresent (paramList  data)
;;Checks whether a parameter was sent
        (cond ((not (eq 0 (list-length paramList)))
                    (cond ((string-equal (car paramList) data)
                                t)
                          (t (paramPresent (cdr paramList) data))))
            (t nil)))

(defun createCommand (paramList data commandFormat newCommand)
      (cond ((not (eq 0 (list-length commandFormat)))
                  (cond ((paramPresent paramList (car commandFormat))
                              (createCommand paramList data (cdr commandFormat) (append newCommand (list (nth (paramPos paramList (car commandFormat) 0 )  data)  ) )))
                        (t (createCommand paramList data (cdr commandFormat) (append newCommand (list "nil") )))))
            (t newCommand)))

(defun formatEntry  (paramList data commandFormat)
;;If an insert operation has a parentesis to indicate that not all records are available it returns the command in a formated form
      (cond ((paramPresent paramList (car commandFormat))
                  (createCommand paramList data commandFormat '()))
        (t  (print "no se encuentra llave"))))

(defun newRecord (command dataBase)
;;Adds a new record into the dataBase, this assumes that the command being recieved already recieves the data in the correct format
      (cond ((listp (cadr command))
                  (newRecord (cons (car command) (formatEntry (cadr command) (cddr command) (cdr (car (returnTable (car command) (car dataBase)))) )) dataBase))
            (t (cond ( (tableExists (car command) (car dataBase));;Checks existance of the table
            ;;Appends the dataBase minus the modified table and the modified table with the new record
                        (cond ( (isKeyAvaliable (cadr command) (returnTable (car command) (car dataBase)))
                              (cond ( (checkForeignKeys (referencesFromTable (car command) (cadr dataBase) '())  command (car dataBase))
                                            (append
                                                    (list (append (append (list (appendNewRecord (cdr command) (returnTable (car command) (car dataBase)))))
                                                    (tableDelete (car command) (car dataBase) '())))
                                                    (cdr dataBase)))
                                            (t (addRecordError "The foreign key doesn't exist" dataBase))))
                              (t  (addRecordError "privateKey already in use" dataBase))))
                      (t (addRecordError "Requested table doesn't exist" dataBase))))));;If the table doesn't exist no action is executed))
      
(defun recordDelete (privateKey Table newTable)
;;Recieves the table without the information entry, and deletes a record
      (cond ((> (list-length Table) 0)
            (cond ((string-equal (caar Table) privateKey)
                        (append newTable (cdr Table)))
                  (t 
                        (recordDelete privateKey (cdr Table) (append newTable (list (car Table)))))
            ))
            (t newTable)))

(defun recordDeleterError (error dataBase)
    (print error)
    (cond (t database)))

(defun recordDeleter (command dataBase)
;;Deletes a record when requested by the user, doesn't verify the reference by a foreign record since this still hasn't been implemented
      (cond  ( (tableExists (car command) (car dataBase)) 
                  (cond ((not (isKeyAvaliable (cadr command) (returnTable (car command) (car dataBase)))) 
                              ( append (list (append (tableDelete (car command) (car dataBase) '())
                                                      (list (recordDelete (cadr command) (returnTable (car command) (car dataBase)) '() ))))
                                        (cdr dataBase))
                              )
                        (t (recordDeleterError "sorry, the requested record doesn't exist" dataBase)) ))
            (t (recordDeleterError "sorry, the requested table doesn't exist" dataBase))))

(defun tableDelete (tableName dataBase newDB)
;;Deletes a table if it exists inside of the dataBase. The newDB is used to storing the values that have already been evaluated, on the first call it must be an empty list
;;This function doesn verify if the specific table is empty
      (cond ((> (list-length dataBase) 0)
            (cond ((string-equal (caaar dataBase) tableName)
                        (append newDB (cdr dataBase)))
                  (t 
                        (tableDelete tableName (cdr dataBase) (append newDB (list (car dataBase)))))))
            (t newDB)))

(defun tableDeleteError (error dataBase)
    (print error)
    (cond (t dataBase)))

(defun isTableReferenced (tableName referenceTable)
;;Checks if a reference points to this table
      (cond ((not (eq 0 (list-length referenceTable)));;Have all references been checked
                (cond ((string-equal tableName (caddar referenceTable)) ;; Is the table referenced in the first reference
                            t)
                      (t  (isTableReferenced tableName (cdr referenceTable)))));;Repeat for the next reference
            (t nil)));;Table hasn't been referenced

(defun tableDeleter (command dataBase)
;;Deletes a table based upon data given by the user
      (cond ((tableExists (car command) (car dataBase))
                    (cond ((isTableReferenced (car command) (cadr  dataBase))
                                    (tableDeleteError "You must delete the reference to this table before deleting it"  database))
                            (t  (cond ((< (list-length (returnTable (car command) (car dataBase))) 2)
                                        (append (list (tableDelete (car command) (car dataBase) '()))
                                                (cdr dataBase)))
                                      (t (tableDeleteError "you must empty the table before deleting it" dataBase))))))
            (t (tableDeleteError "Sorry the table doesn't exist" dataBase))))

(defun tableExists (tableName dataBase)
;; Checks if the name of the table has already been used in an existing table
           (cond ((> (list-length database) 0)
               (cond ((string-equal tableName (caaar Database))  t);;Table name already used, dataBase is composed of tables, first entry in table represents its composition, first entry of the composition is the name
                     (t (tableExists tableName (cdr Database)))))    ;;Checks for the next table in the DB
           (t nil)));;dataBase is empty, table doesn't exist

(defun addTableError (dataBase) 
;;Called when an insertion into the database fails
    (print "Invalid name for table, already in use")
    (cond (t dataBase)))

(defun addTable (command dataBase)
;;Adds a new table to the dataBase as long as it contains the required fields, recieves the information without the addt/addtable command
     (cond ((tableExists (car command) dataBase) (addTableError dataBase))
           (t (append dataBase (list (list command))))))

(defun queryError (error Database)
      (print error)
      (cond (t Database)))

(defun printFilteredEntry (record column)
;;Prints the selected columns from a record
      (cond ((not (eq 0 (list-length column)))
                    (progn
                          (format t "  ~a  |" (nth (car column) record))
                          (printFilteredEntry record (cdr column)  )))
            (t (format t "~%"))))

(defun showFilteredTable (table count filteredColumns)
;;Shows all of the records in a table and all of its columns
      (cond ((eq 0 count)
                    (progn
                          (format t "~a ~%" (caar table));;Table title
                          (printFilteredEntry (cdar table) filteredColumns) ;;
                          (showFilteredTable (cdr table) 1 filteredColumns)))
              (t (cond ((eq 0 (list-length table))
                    (format t "~%"))
              (t (progn
                    (printFilteredEntry (car table) filteredColumns)
                    (showFilteredTable (cdr table) count filteredColumns)))))))

(defun columnPosition (tableTitle column pos)
;;Returns the position in a record in which a column is located
      (cond ((not (eq 0 (list-length tableTitle)))
                  (cond ((string-equal column (car TableTitle))
                        pos)
                  (t (columnPosition (cdr tableTitle) column (+ pos 1)))))
            (t -1)))

(defun filteredColumnsPos (TableTitle requestedColumns filteredColumns)
;;On a table returns the positions on which the 
      ( cond ((eq 0 (list-length requestedColumns))
                    filteredColumns)
              (t (filteredColumnsPos TableTitle
                                    (cdr requestedColumns)
                                    (append filteredColumns (list (- (columnPosition TableTitle (car requestedColumns) 0) 1)))))))

(defun printEntry (record) 
;;Returns one record on the commandline, prints with all the columns
      (cond ((eq 0 (list-length record))
                    (format t "~%"))
            (t (progn 
                    (format t "  ~a  |" (car record))
                    (printEntry (cdr record))))))

(defun showTable (table count)
;;Shows all of the records in a table and all of its columns
;;Count is used in order to print the table name and its title in the correct order
      (cond ((eq 0 count)
                    (progn
                          (format t "~a ~%" (caar table))
                          (printEntry (cdar table))
                          (showTable (cdr table) 1)))
              (t (cond ((eq 0 (list-length table))
                    (format t "~%"))
              (t (progn
                    (printEntry (car table))
                    (showTable (cdr table) count)))))))

(defun filterRecord (record filter pos)
;;Determines if a record must be conserved for the final query
      (cond ((string-equal (car (nthcdr (- pos 1) record)) filter)
                  t)
            (t nil)))

(defun filterTable (newTable oldTable columnPos filter)
;;Takes a table and a filter for one of its columns and returns the table without those columns
;;columnPos is zero-indexedr
    (cond ((not (eq 0 (list-length oldTable)))
                ( cond ((filterRecord (car oldTable) filter columnPos)
                            (filterTable (append newTable (list (car oldTable)))  (cdr oldTable)  columnPos filter))
                      (t (filterTable newTable (cdr oldTable)  columnPos filter)))) 
      (t newTable)))

(defun tableFilter (oldTable filters tableTitle)
;;Applies filterTable for several tables
      (cond ((eq 0 (list-length filters))
                  oldTable)
            (t (tableFilter (filterTable '() oldTable (columnPosition TableTitle (car filters) 0) (cadr filters)) (cddr filters) tableTitle))))

(defun query (command dataBase)
;;Recieves queries from users and executes the correct action
  (progn
      (cond ((tableExists (car command) (car dataBase));;Checks existance of the queried table 
                (cond ((eq 0 (list-length (cdr command))) ;;If there aren't more commands, print all the table
                            (showTable (returnTable (car command) (car dataBase)) 0))
                      (t
                        (cond ((not (eq 0 (list-length (cddr command))) ) ;; If there are filters
                                  (cond ((string-equal "all" (caadr command));;Wants to see all columns with filters
                                              (showTable (append (list (car (returnTable (car command) (car dataBase))))
                                                                (tableFilter 
                                                                    (returnTable (car command) (car dataBase))
                                                                    (cddr command)
                                                                    (car (returnTable (car command) (car dataBase))))) 0))
                                      ;;User wants to see some coluns with filters
                                      (t (showFilteredTable (append (list (car (returnTable (car command) (car dataBase))))
                                                                (tableFilter 
                                                                    (returnTable (car command) (car dataBase))
                                                                    (cddr command)
                                                                    (car (returnTable (car command) (car dataBase)))))
                                                            0
                                                            (filteredColumnsPos (car (returnTable (car command) (car dataBase))) (cadr command) '())))))
                                 ;;User wants some columns without any filters applied
                                (t (showFilteredTable (returnTable (car command) (car dataBase))
                                                            0
                                                            (filteredColumnsPos (car (returnTable (car command) (car dataBase))) (cadr command) '())))))))
            (t (queryError "Table doesn't exist") ))
      (cond (t dataBase))))


(defun ProcedureError (error dataBase)
    (print error)
    (cond (t dataBase))
)

(defun procedureExists (procedureName procedureTable)
;; Checks if the name of the procedure has already been used
           (cond ((> (list-length procedureTable) 0)
               (cond ((string-equal procedureName (caar procedureTable))  t);;Table name already used
                     (t (procedureExists procedureName (cdr procedureTable)))))    ;;Checks for the next table in the procedureTable
           (t nil)));;procedureTable is empty, procedureName doesn't exist


(defun returnProcedure (procedureName procedureTable)
;;Returns a specific procedure, existance of the table has already been proved
      (cond ((string-equal procedureName (caar procedureTable))   (car procedureTable));;Procedure name found
            (t (returnProcedure procedureName (cdr procedureTable)))))  ;;Checks for the next table in the DB;;dataBase is empty, table doesn't exist

(defun createProcedure (command dataBase)
;;Stores a procedure in the database
    (cond ((not (procedureExists (car command) (caddr dataBase)))
          (append (list (car dataBase)) (list (cadr dataBase))  (list (append (caddr dataBase) (list command)))  ))
      (t (ProcedureError "Function name already used" dataBase)))
)

(defun replaceOneParam (oldCommand paramName newData newCommand)
;;Takes a command where all appearances of paramName will be changed by newData
  (cond ((not (eq 0 (list-length oldCommand)))
          (cond ((stringp (car oldCommand))
                  (cond ((string-equal paramName (car oldCommand))
                           (replaceOneParam (cdr oldCommand) paramName newData  (append  newCommand  (list newData))))
                         (t (replaceOneParam (cdr oldCommand) paramName newData (append  newCommand  (list (car oldCommand)))))))
                 (t (replaceOneParam (cdr oldCommand) paramName newData  (append  newCommand  (list (replaceOneParam (car oldCommand) paramName newData '())))) )))
      (newCommand)))

(defun replaceParams (dataToReplace newData command)
;;Takes a list of columnNames, data to replace them with and a command where the changes will be executed
  (cond ((not (eq 0 (list-length dataToReplace)))
          (replaceParams (cdr dataToReplace) (cdr newData) (replaceOneParam command (car dataToReplace) (car newData) '()) ))
      (t command))
)


(defun evalProcedureError (error dataBase)
    (print error)
    (readFromUser dataBase)
)


(defun evaluateProcedure (command dataBase)
;;Evaluates an stored command with data provided by the user
  (cond ((procedureExists (car command) (caddr dataBase))
        (cond ((eq (list-length (cdr command)) (list-length (cadr (returnProcedure (car command) (caddr dataBase))))  )
                (functSelect (replaceParams (cadr (returnProcedure (car command) (caddr dataBase))) (cdr command) (cddr (returnProcedure (car command) (caddr dataBase)))) dataBase)) 
            (t (evalProcedureError "Not enough paramaters provided" dataBase))))
    (t (evalProcedureError "Requested Function doesn't exist" dataBase)))
)



(defun functSelect (command dataBase)
;;Reading the first parameter from de CLI it decides which function is being recieved and acts accordingly
     (cond ((or (string-equal (car command) "addt") (string-equal (car command) "addtable"))        (readFromUser (append (list (addTable (cdr command) (car dataBase))) (cdr dataBase))))
           ((or (string-equal (car command) "addr") (string-equal (car command) "addReference"))    (readFromUser (referenceAdder   (cdr command) dataBase)))
           ((or (string-equal (car command) "remr") (string-equal (car command) "removeReference")) (readFromUser (referenceDeleter (cdr command) dataBase)))
           ((or (string-equal (car command) "ins")  (string-equal (car command) "insert"))          (readFromUser (newRecord        (cdr command) dataBase)))
           ((or (string-equal (car command) "ud")   (string-equal (car command) "update"))          (readFromUser (recordModifier   (cdr command) dataBase)))
           ((or (string-equal (car command) "rr")   (string-equal (car command) "remover"))         (readFromUser (recordDeleter    (cdr command) dataBase)))
           ((or (string-equal (car command) "dt")   (string-equal (car command) "deltable"))        (readFromUser (tableDeleter     (cdr command) dataBase)))
           ((string-equal (car command) "cproc")                                                    (readFromUser (createProcedure  (cdr command) dataBase)))
           ((string-equal (car command) "eval")                                                     (evaluateProcedure (cdr command) dataBase))
           ((string-equal (car command) "query")                                                    (readFromUser (query (cdr command) dataBase)))
           ((string-equal (car command) "showall")                                                  (print "Mostrando toda la DB"))
           (t (progn (print "Unknown command") (readFromUser dataBase)))))

(defun my-split (string &key (delimiterp #'delimiterp))
;; Splits a string into a list containing substrings, this division is stablished by delimiterp
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defun delimiterp (c) (or (char= c #\Space)))
;; Marks which characters work as a delimiter when revieving input from the CL

(defun replace-all (string part replacement &key (test #'char=))
;;"Returns a new string in which all the occurences of the part is replaced with replacement
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun finalParenthesis (start end)
;;Searches the rest of the request for the ending parenthesis, if not found it is assumed that after the parenthesis everything is part of the list
      (cond ((not (eq 0 (length end)))
            (cond ( (string-equal ")" (char (car end) (- (length (car end)) 1)))
                        (append start  (list (replace-all (car end) ")" ""))))
                  (t (finalParenthesis (append start (list (car end))) (cdr end) ))))
            (t start)))

(defun listAfterClosingParenthesis (command)
;;Returns the list without the processed part in finalParenthesis
      (cond ((not (eq 0 (length command)))
               (cond ( (string-equal ")" (char (car command) (- (length (car command)) 1)))
                        (cdr command))
                  (t (listAfterClosingParenthesis (cdr command)  ))))
          (t '())))

(defun parenthesisRestore (oldString newString) 
;;my-split doesn't conserve the union created by the parenthesis, this functions restores this and joins them in a list
     (cond ((not (eq 0 (length OldString)))
               (cond ((string-equal "(" (char (car oldString) 0))
                         (parenthesisRestore (listAfterClosingParenthesis  oldString) 
                                (append newString (list (finalParenthesis '() (cons (replace-all (car oldString) "(" "") (cdr oldString))))))
                     )
                    (t (parenthesisRestore (cdr oldString) (append newString (list (car oldString)))))))
          (t newString)))

(defun my-read () (parenthesisRestore (my-split (READ-LINE)) '()))


(defun readFromUser (dataBase)
;; Cicle that mantains the dataBase in the stack
      (print dataBase)
      (format t "~%")
     (functSelect (my-read) dataBase))

(readFromUser '(((("institution" "id" "name" "location"))
  (("person" "id" "name" "lastname" "gender")))
 NIL
 (("InsertInstitution" ("ident" "loc" "n") "insert" "institution"
   ("id" "name" "location") "ident" "n" "loc")
  ("AddPerson" ("ced" "gen" "n" "lastn") "insert" "person"
   ("id" "name" "lastname" "gender") "ced" "n" "lastn" "gen")))   )
