;;; key-mode.el --- A mode for KeY files

;; Copyright (C) 2019 Alexander Weigl <weigl@kit.edu>
;; URL: https://github.com/wadoon/key-mode
;; Version: 0.1
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))
;; Package-Version: 0

;;; Usage:
;; Put the following code in your .emacs, site-load.el, or other relevant file
;; (add-to-list 'load-path "path-to-key-mode")
;; (require 'key-mode)

;;; Commentary:
;; This file provides the support for the rules and problems of the
;; KeY Theorem Prover `https://key-project.org'.
;;
;;
;; It provides following features:
;; 
;; - syntax highlighting 
;; - auto completion of keywords and sorts, functions, and predicates defined in the ldts
;; - snippets

;;; License:
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'cl-lib)


(defvar key-mode-keymap nil
  "The mode map for key-mode.")

(defvar key-mode-text-mode-syntax-table nil
  "The syntax table of key-mode.")

(defvar key-mode-font-lock nil)

(setq key-mode--keywords
      (list "\\sorts" "\\generic" "\\proxy" "\\extends" "\\oneof" "\\abstract"
            "\\schemaVariables" "\\schemaVar" "\\modalOperator" "\\program"
            "\\formula" "\\term" "\\update" "\\variables" "\\variable" "\\skolemTerm"
            "\\skolemFormula" "\\termlabel" "\\modifies" "\\programVariables" "\\sameObserver"
            "\\varcond" "\\applyUpdateOnRigid" "\\dependingOn" "\\disjointModuloNull"
            "\\dropEffectlessElementaries" "\\dropEffectlessStores" "\\simplifyIfThenElseUpdate"
            "\\enumConstant" "\\freeLabelIn" "\\hasSort" "\\fieldType" "\\final" "\\elemSort" "\\hasLabel"
            "\\hasSubFormulas" "\\isArray" "\\isArrayLength" "\\isConstant" "\\isEnumType"
            "\\isInductVar" "\\isLocalVariable" "\\isObserver" "\\different" "\\metaDisjoint"
            "\\isThisReference" "\\differentFields" "\\isReference" "\\isReferenceArray"
            "\\isStaticField" "\\sub" "\\equalUnique" "\\new" "\\newLabel" "\\containsAssignment"
            "\\not" "\\notFreeIn" "\\same" "\\static" "\\staticMethodReference" "\\mayExpandMethod"
            "\\strict" "\\typeof" "\\instantiateGeneric" "\\forall" "\u2200" "\\exists"
            "\u2203" "\\subst" "\\if" "\\ifEx" "\\then" "\\else" "\\include" "\\includeLDTs" "\\classpath"
            "\\bootclasspath" "\\noDefaultClasses" "\\javaSource" "\\withOptions" "\\optionsDecl"
            "\\settings" "\\profile" "true" "false" "\\sameUpdateLevel" "\\inSequentState"
            "\\antecedentPolarity" "\\succedentPolarity" "\\closegoal" "\\heuristicsDecl"
            "\\noninteractive" "\\displayname" "\\helptext" "\\replacewith" "\\addrules"
            "\\addprogvars" "\\heuristics" "\\find" "\\add" "\\assumes" "\\trigger"
            "\\avoid" "\\predicates" "\\functions" "\\transformers" "\\unique" "\\rules"
            "\\axioms" "\\problem" "\\chooseContract" "\\proofObligation" "\\proof"
            "\\proofScript" "\\contracts" "\\invariants" "\\lemma" "\\inType"
            "\\isAbstractOrInterface" "\\containerType" "\\locset" "\\seq" "\\bigint"
            "\u227A" "\u220A" "\u2205" "\u222A" "\u2229" "\u2286" "\u2216"))

;; (defvar key-mode-toplevel-keywords nil "Toplevel keywords.")
;; (defvar key-mode-taclet-keywords nil "Taclet keywords.")

;; (regexp-opt '("\\javaSource"
;; 	       "\\chooseContract"
;; 	       "\\proofObligation"
;; 	       "\\bootclasspath"
;; 	       "\\problem"
;; 	       "\\include"
;;                "\\includeldt"
;; 	       "\\sorts"
;;                "\\profile"
;;                "\\preferences"
;;                "\\predicates"
;;                "\\functions"
;;                "\\withoptions"
;; 	       "\\programVariables")
;; 	      t))
;; (regexp-opt '("\\find" "\\assumes"
;;       	"\\modality"
;;       	"\\replacewith"
;;       	"\\add"
;;       	"\\heuristics"
;;       	"\\endmodality"
;;       	"\\varcond"
;;       	"\\not"
;;       	"\\hasSort"
;;       	"\\isThisReference"
;;       	"\\staticMethodReference"
;;       	"\\displayname"
;;       	"\\sameUpdateLevel"
;;       	"schemaVar"
;;       	"\\modalOperator"
;;       	"\\new"
;;       	"\\typeof"
;;       	"\\term"
;;       	"\\update"
;;       	"\\formula")
;;             t))

(setq key-mode-font-lock
      (let ((keywords-re (regexp-opt key-mode--keywords t)))
        `((,keywords-re . font-lock-keyword-face)
          ;; TODO #.* scheme variables
	  ;; TODO group (toplevel, rule level)
	  ;;("FIXME" . show-paren-mismatch-face)
          ;;(,key-mode-toplevel-keywords . font-lock-function)
       )))

(setq key-mode-syntax-table
      (let ((table (make-syntax-table)))
	;; ' is a string delimiter
	;;(modify-syntax-entry ?' "\"" table)
	;; " is a string delimiter too
	(modify-syntax-entry ?\" "\"" table)

	(modify-syntax-entry ?\\ "_" table)
	
	;; / is punctuation, but // is a comment starter
	(modify-syntax-entry ?/ ". 12" table)
	;; \n is a comment ender
	(modify-syntax-entry ?\n ">" table)
	table))

;; Create the keymap for this mode.
(setq key-mode-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-derived-mode key-mode prog-mode "KeY"
  "A major mode for editing KeY files"
  :syntax-table key-mode-syntax-table


  ;(set (make-local-variable 'text-mode-variant) t)
  ;(set (make-local-variable 'require-final-newline') mode-require-final-newline)
  ;(set (make-local-variable 'indent-line-function) 'indent-relative)
  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (set (make-local-variable 'font-lock-defaults) '(key-mode-font-lock))
  (setq font-lock-keywords key-mode-font-lock)
  (font-lock-fontify-buffer))


;; Flycheck support
(defun key-mode--flycheck ()
  (flycheck-define-checker
   key
   "A KeY syntax checker using the fatJar of KeY. To override the
path to the jar file, set `flycheck-key-executable'.  See URL
`http://key-project.org'."
   :command ("pyflakes" source-inplace)
   :error-patterns
   ((error line-start (file-name) ":" line ":" (message) line-end))
   :modes key-mode)

  (add-to-list 'flycheck-checkers 'key))

(eval-after-load 'flycheck #'key-mode--flycheck)
;;

(setq key-mode--completions
      `(;; ("keywords" .
	,@(mapcar (lambda (x) (list x "keyword" "")) key-mode--keywords)
	
	;;("sorts"
	("Field" "Sort" "heap.key:4")
	("Heap" "Sort" "heap.key:5")
	("LocSet" "Sort" "locSets.key:4")
	("Map" "Sort" "map.key:19")
	("Pair" "Sort" "wellfound.key:31")
	("Permission" "Sort" "permission.key:15")
	("PermissionOwnerList" "Sort" "permission.key:16")
	("RegEx" "Sort" "regExHeader.key:1")
	("Seq" "Sort" "seq.key:21")
	("\abstract java.io.Serializable \extends java.lang.Object" "Sort" "ruleSetsDeclarations.key:21")
	("\abstract java.lang.Cloneable \extends java.lang.Object" "Sort" "ruleSetsDeclarations.key:20")
	("any" "Sort" "ruleSetsDeclarations.key:18")
	("boolean" "Sort" "boolean.key:14")
	("float, double, real" "Sort" "integerHeader.key:7")
	("int" "Sort" "integerHeader.key:2")
	("java.lang.Object" "Sort" "ruleSetsDeclarations.key:19")
	("numbers" "Sort" "integerHeader.key:1")
	
	;; ("functions" .
	("TRUE" "boolean TRUE" "boolean.key:19")
	("FALSE" "boolean FALSE" "boolean.key:20")
	("clIndexOfChar" "int clIndexOfChar(Seq, int, int)" "charListHeader.key:1")
	("clIndexOfCl" "int clIndexOfCl(Seq,int,Seq)" "charListHeader.key:3")
	("clLastIndexOfChar" "int clLastIndexOfChar(Seq, int, int)" "charListHeader.key:4")
	("clLastIndexOfCl" "int clLastIndexOfCl(Seq, int, Seq)" "charListHeader.key:5")
	("clReplace" "Seq clReplace (Seq, int, int)" "charListHeader.key:7")
	("clTranslateInt" "Seq clTranslateInt(int)" "charListHeader.key:8")
	("clRemoveZeros" "Seq clRemoveZeros(Seq)" "charListHeader.key:9")
	("clHashCode" "int clHashCode(Seq)" "charListHeader.key:10")
	("alpha::select" "alpha alpha::select(Heap, Object, Field)" "heap.key:12")
	("store" "Heap store(Heap, Object, Field, any)" "heap.key:13")
	("create" "Heap create(Heap, Object)" "heap.key:14")
	("anon" "Heap anon(Heap, LocSet, Heap)" "heap.key:15")
	("memset" "Heap memset(Heap, LocSet, any)" "heap.key:16")
	("alpha::defaultValue" "alpha alpha::defaultValue" "heap.key:19")
	("arr" "Field arr(int)" "heap.key:22")
	("java.lang.Object::<transient>" " Field java.lang.Object::<transient>" "heap.key:23")
	("java.lang.Object::<transactionConditionallyUpdated>" " Field java.lang.Object::<transactionConditionallyUpdated>" "heap.key:24")
	("java.lang.Object::<created>" " Field java.lang.Object::<created>" "heap.key:25")
	("Field java.lang.Object::<initialized>" "Field java.lang.Object::<initialized>" "heap.key:26")
	("Field alpha::<classPrepared>" "Field alpha::<classPrepared>" "heap.key:27")
	("Field alpha::<classInitialized>" "Field alpha::<classInitialized>" "heap.key:28")
	("alpha::<classInitializationInProgress>" " Field alpha::<classInitializationInProgress>" "heap.key:29")
	("alpha::<classErroneous>" " Field alpha::<classErroneous>" "heap.key:30")
	("length" "int length(Object)" "heap.key:33")
	("null" "Null null" "heap.key:36")
	("#" "numbers #" "integerHeader.key:19")
	("0" "numbers 0 (numbers)" "integerHeader.key:20")
	("1" "numbers 1 (numbers)" "integerHeader.key:21")
	("2" "numbers 2 (numbers)" "integerHeader.key:22")
	("3" "numbers 3 (numbers)" "integerHeader.key:23")
	("4" "numbers 4 (numbers)" "integerHeader.key:24")
	("5" "numbers 5 (numbers)" "integerHeader.key:25")
	("6" "numbers 6 (numbers)" "integerHeader.key:26")
	("7" "numbers 7 (numbers)" "integerHeader.key:27")
	("8" "numbers 8 (numbers)" "integerHeader.key:28")
	("9" "numbers 9 (numbers)" "integerHeader.key:29")
	("neglit" "numbers neglit (numbers)" "integerHeader.key:30")
	("Z" "int Z (numbers)" "integerHeader.key:32")
	("C" "int C (numbers)" "integerHeader.key:33")
	("add" "int add(int,int)" "integerHeader.key:36")
	("neg" "int neg(int)" "integerHeader.key:37")
	("sub" "int sub(int,int)" "integerHeader.key:38")
	("mul" "int mul(int, int)" "integerHeader.key:39")
	("div" "int div(int, int)" "integerHeader.key:40")
	("mod" "int mod(int, int)" "integerHeader.key:41")
	("pow" "int pow(int, int)" "integerHeader.key:42")
	("bsum{false,false,true}" "int bsum{false,false,true}(int, int, int)" "integerHeader.key:45")
	("bprod{false,false,true}" "int bprod{false,false,true}(int, int, int)" "integerHeader.key:46")
	("sum{true,true}" "int sum{true,true}(boolean,int)" "integerHeader.key:47")
	("prod{true,true}" "int prod{true,true}(boolean,int)" "integerHeader.key:48")
	("min{true,true}" "int min{true,true}(boolean,int)" "integerHeader.key:49")
	("max{true,true}" "int max{true,true}(boolean,int)" "integerHeader.key:50")
	("byte_MAX" "int byte_MAX" "integerHeader.key:53")
	("byte_MIN" "int byte_MIN" "integerHeader.key:54")
	("char_MAX" "int char_MAX" "integerHeader.key:55")
	("char_MIN" "int char_MIN" "integerHeader.key:56")
	("int_MAX" "int int_MAX" "integerHeader.key:57")
	("int_MIN" "int int_MIN" "integerHeader.key:58")
	("long_MAX" "int long_MAX" "integerHeader.key:59")
	("long_MIN" "int long_MIN" "integerHeader.key:60")
	("short_MAX" "int short_MAX" "integerHeader.key:61")
	("short_MIN" "int short_MIN" "integerHeader.key:62")
	("byte_HALFRANGE" "int byte_HALFRANGE" "integerHeader.key:65")
	("byte_RANGE" "int byte_RANGE" "integerHeader.key:66")
	("char_RANGE" "int char_RANGE" "integerHeader.key:67")
	("int_HALFRANGE" "int int_HALFRANGE" "integerHeader.key:68")
	("int_RANGE" "int int_RANGE" "integerHeader.key:69")
	("long_HALFRANGE" "int long_HALFRANGE" "integerHeader.key:70")
	("long_RANGE" "int long_RANGE" "integerHeader.key:71")
	("short_HALFRANGE" "int short_HALFRANGE" "integerHeader.key:72")
	("short_RANGE" "int short_RANGE" "integerHeader.key:73")
	("undefinedPow" "int undefinedPow(int, int)" "integerHeader.key:76")
	("javaUnaryMinusIntOverFlow" "int javaUnaryMinusIntOverFlow(int)" "integerHeader.key:79")
	("javaUnaryMinusLongOverFlow" "int javaUnaryMinusLongOverFlow(int)" "integerHeader.key:80")
	("javaAddIntOverFlow" "int javaAddIntOverFlow(int,int)" "integerHeader.key:81")
	("javaAddLongOverFlow" "int javaAddLongOverFlow(int,int)" "integerHeader.key:82")
	("javaSubIntOverFlow" "int javaSubIntOverFlow(int,int)" "integerHeader.key:83")
	("javaSubLongOverFlow" "int javaSubLongOverFlow(int,int)" "integerHeader.key:84")
	("javaMulIntOverFlow" "int javaMulIntOverFlow(int, int)" "integerHeader.key:85")
	("javaMulLongOverFlow" "int javaMulLongOverFlow(int, int)" "integerHeader.key:86")
	("javaModOverFlow" "int javaModOverFlow(int, int)" "integerHeader.key:87")
	("javaDivIntOverFlow" "int javaDivIntOverFlow(int, int)" "integerHeader.key:88")
	("javaDivLongOverFlow" "int javaDivLongOverFlow(int, int)" "integerHeader.key:89")
	("javaUnsignedShiftRightOverFlow" "int javaUnsignedShiftRightOverFlow(int,int)" "integerHeader.key:90")
	("javaBitwiseOrIntOverFlow" "int javaBitwiseOrIntOverFlow(int,int)" "integerHeader.key:91")
	("javaCastByteOverFlow" "int javaCastByteOverFlow(int)" "integerHeader.key:92")
	("javaCastShortOverFlow" "int javaCastShortOverFlow(int)" "integerHeader.key:93")
	("javaCastIntOverFlow" "int javaCastIntOverFlow(int)" "integerHeader.key:94")
	("javaCastLongOverFlow" "int javaCastLongOverFlow(int)" "integerHeader.key:95")
	("javaCastCharOverFlow" "int javaCastCharOverFlow(int)" "integerHeader.key:96")
	("jmod" "int jmod(int, int)" "integerHeader.key:99")
	("jdiv" "int jdiv(int, int)" "integerHeader.key:100")
	("unaryMinusJint" "int unaryMinusJint(int)" "integerHeader.key:101")
	("unaryMinusJlong" "int unaryMinusJlong(int)" "integerHeader.key:102")
	("addJint" "int addJint(int,int)" "integerHeader.key:103")
	("addJlong" "int addJlong(int,int)" "integerHeader.key:104")
	("subJint" "int subJint(int,int)" "integerHeader.key:105")
	("subJlong" "int subJlong(int,int)" "integerHeader.key:106")
	("mulJint" "int mulJint(int, int)" "integerHeader.key:107")
	("mulJlong" "int mulJlong(int, int)" "integerHeader.key:108")
	("modJint" "int modJint(int, int)" "integerHeader.key:109")
	("modJlong" "int modJlong(int, int)" "integerHeader.key:110")
	("divJint" "int divJint(int, int)" "integerHeader.key:111")
	("divJlong" "int divJlong(int, int)" "integerHeader.key:112")
	("moduloByte" "int moduloByte(int)" "integerHeader.key:113")
	("moduloShort" "int moduloShort(int)" "integerHeader.key:114")
	("moduloInt" "int moduloInt(int)" "integerHeader.key:115")
	("moduloLong" "int moduloLong(int)" "integerHeader.key:116")
	("moduloChar" "int moduloChar(int)" "integerHeader.key:117")
	("shiftright" "int shiftright(/*left*/ int, /*right*/ int)" "integerHeader.key:125")
	("shiftleft" "int shiftleft(/*left*/ int, /*right*/int)" "integerHeader.key:127")
	("unsignedshift" "int unsignedshift(/*left*/ int, /*right*/int, /*bitsize*/ int)" "integerHeader.key:131")
	("shiftrightJint" "int shiftrightJint(int, int)" "integerHeader.key:135")
	("shiftleftJint" "int shiftleftJint(int, int)" "integerHeader.key:136")
	("shiftleftJlong" "int shiftleftJlong(int, int)" "integerHeader.key:138")
	("shiftrightJlong" "int shiftrightJlong(int, int)" "integerHeader.key:139")
	("unsignedshiftrightJint" "int unsignedshiftrightJint(int, int)" "integerHeader.key:141")
	("unsignedshiftrightJlong" "int unsignedshiftrightJlong(int, int)" "integerHeader.key:142")
	("binaryAnd" "int binaryAnd(/*left*/ int, /*right*/ int)" "integerHeader.key:146")
	("binaryOr" "int binaryOr(/*left*/ int, /*right*/ int)" "integerHeader.key:147")
	("binaryXOr" "int binaryXOr(/*left*/ int, /*right*/ int)" "integerHeader.key:148")
	("orJint" "int orJint(int, int)" "integerHeader.key:152")
	("orJlong" "int orJlong(int, int)" "integerHeader.key:153")
	("andJint" "int andJint(int, int)" "integerHeader.key:154")
	("andJlong" "int andJlong(int, int)" "integerHeader.key:155")
	("xorJint" "int xorJint(int, int)" "integerHeader.key:156")
	("xorJlong" "int xorJlong(int, int)" "integerHeader.key:157")
	("index" "int index" "integerHeader.key:162")
	("javaUnaryMinusInt" "int javaUnaryMinusInt(int)" "integerHeader.key:170")
	("javaUnaryMinusLong" "int javaUnaryMinusLong(int)" "integerHeader.key:171")
	("javaBitwiseNegation" "int javaBitwiseNegation(int)" "integerHeader.key:172")
	("javaAddInt" "int javaAddInt(int,int)" "integerHeader.key:173")
	("javaAddLong" "int javaAddLong(int,int)" "integerHeader.key:174")
	("javaSubInt" "int javaSubInt(int,int)" "integerHeader.key:175")
	("javaSubLong" "int javaSubLong(int,int)" "integerHeader.key:176")
	("javaMulInt" "int javaMulInt(int, int)" "integerHeader.key:177")
	("javaMulLong" "int javaMulLong(int, int)" "integerHeader.key:178")
	("javaMod" "int javaMod(int, int)" "integerHeader.key:179")
	("javaDivInt" "int javaDivInt(int, int)" "integerHeader.key:180")
	("javaDivLong" "int javaDivLong(int, int)" "integerHeader.key:181")
	("javaShiftRightInt" "int javaShiftRightInt(int, int)" "integerHeader.key:182")
	("javaShiftRightLong" "int javaShiftRightLong(int, int)" "integerHeader.key:183")
	("javaShiftLeftInt" "int javaShiftLeftInt(int, int)" "integerHeader.key:184")
	("javaShiftLeftLong" "int javaShiftLeftLong(int, int)" "integerHeader.key:185")
	("javaUnsignedShiftRightInt" "int javaUnsignedShiftRightInt(int, int)" "integerHeader.key:186")
	("javaUnsignedShiftRightLong" "int javaUnsignedShiftRightLong(int, int)" "integerHeader.key:187")
	("javaBitwiseOrInt" "int javaBitwiseOrInt(int, int)" "integerHeader.key:188")
	("javaBitwiseOrLong" "int javaBitwiseOrLong(int, int)" "integerHeader.key:189")
	("javaBitwiseAndInt" "int javaBitwiseAndInt(int, int)" "integerHeader.key:190")
	("javaBitwiseAndLong" "int javaBitwiseAndLong(int, int)" "integerHeader.key:191")
	("javaBitwiseXOrInt" "int javaBitwiseXOrInt(int, int)" "integerHeader.key:192")
	("javaBitwiseXOrLong" "int javaBitwiseXOrLong(int, int)" "integerHeader.key:193")
	("javaCastByte" "int javaCastByte(int)" "integerHeader.key:194")
	("javaCastShort" "int javaCastShort(int)" "integerHeader.key:195")
	("javaCastInt" "int javaCastInt(int)" "integerHeader.key:196")
	("javaCastLong" "int javaCastLong(int)" "integerHeader.key:197")
	("javaCastChar" "int javaCastChar(int)" "integerHeader.key:198")
	("empty" "LocSet empty" "locSets.key:9")
	("allLocs" "LocSet allLocs" "locSets.key:10")
	("singleton" "LocSet singleton(Object, Field)" "locSets.key:13")
	("union" "LocSet union(LocSet, LocSet)" "locSets.key:14")
	("intersect" "LocSet intersect(LocSet, LocSet)" "locSets.key:15")
	("setMinus" "LocSet setMinus(LocSet, LocSet)" "locSets.key:16")
	("infiniteUnion{true}" "LocSet infiniteUnion{true}(LocSet)" "locSets.key:17")
	("allFields" "LocSet allFields(Object)" "locSets.key:18")
	("allObjects" "LocSet allObjects(Field)" "locSets.key:19")
	("arrayRange" "LocSet arrayRange(Object, int, int)" "locSets.key:20")
	("freshLocs" "LocSet freshLocs(Heap)" "locSets.key:21")
	("allElementsOfArray" "LocSet allElementsOfArray(Heap, Object, LocSet)" "locSets.key:23")
	("allElementsOfArrayLocsets" "LocSet allElementsOfArrayLocsets(Heap, Object, LocSet)" "locSets.key:24")
	("mapGet" "any mapGet(Map, any)" "map.key:25")
	("mapUndef" "any mapUndef" "map.key:26")
	("mapForeach{true,true}" "Map mapForeach{true,true}(boolean, any)" "map.key:29")
	("mapEmpty" "Map mapEmpty" "map.key:30")
	("mapSingleton" "Map mapSingleton(any, any)" "map.key:31")
	("mapOverride" "Map mapOverride(Map, Map)" "map.key:32")
	("seq2map" "Map seq2map(Seq)" "map.key:33")
	("mapUpdate" "Map mapUpdate(Map, any, any)" "map.key:34")
	("mapRemove" "Map mapRemove(Map, any)" "map.key:35")
	("mapSize" "int mapSize(Map)" "mapSize.key:25")
	("currentThread" "Object currentThread" "permission.key:20")
	("//" " // distinct object representing" "permission.key:20")
	("emptyPermissionOwnerList" "PermissionOwnerList emptyPermissionOwnerList" "permission.key:24")
	("consPermissionOwnerList" "PermissionOwnerList consPermissionOwnerList(Object,PermissionOwnerList)" "permission.key:25")
	("insertPermissionOwner" "PermissionOwnerList insertPermissionOwner(Object,Object,int,PermissionOwnerList)" "permission.key:28")
	("returnPermissionOwner" "PermissionOwnerList returnPermissionOwner(Object, PermissionOwnerList)" "permission.key:29")
	("emptyPermission" "Permission emptyPermission" "permission.key:34")
	("slice" "Permission slice(PermissionOwnerList,Permission)" "permission.key:35")
	("transferPermission" "Permission transferPermission(boolean,Object,Object,int,Permission)" "permission.key:38")
	("returnPermission" "Permission returnPermission(Object,Object,Permission)" "permission.key:39")
	("initFullPermission" "Permission initFullPermission" "permission.key:42")
	("owner1" "PermissionOwnerList owner1(Object)" "permission.key:43")
	("owner2" "PermissionOwnerList owner2(Object,Object)" "permission.key:44")
	("owner3" "PermissionOwnerList owner3(Object,Object,Object)" "permission.key:45")
	("owner4" "PermissionOwnerList owner4(Object,Object,Object,Object)" "permission.key:46")
	("slice1" "Permission slice1(PermissionOwnerList)" "permission.key:48")
	("slice2" "Permission slice2(PermissionOwnerList,PermissionOwnerList)" "permission.key:49")
	("regEx" "RegEx regEx(Seq)" "regExHeader.key:5")
	("opt" "RegEx opt(RegEx)" "regExHeader.key:6")
	("alt" "RegEx alt(RegEx, RegEx)" "regExHeader.key:7")
	("regExConcat" "RegEx regExConcat(RegEx, RegEx)" "regExHeader.key:8")
	("repeat" "RegEx repeat(RegEx, int)" "regExHeader.key:10")
	("repeatStar" "RegEx repeatStar(RegEx)" "regExHeader.key:11")
	("repeatPlus" "RegEx repeatPlus(RegEx)" "regExHeader.key:12")
	("alpha::cast" "alpha alpha::cast(any)" "ruleSetsDeclarations.key:28")
	("alpha::exactInstance" "boolean alpha::exactInstance(any)" "ruleSetsDeclarations.key:29")
	("alpha::instance" "boolean alpha::instance(any)" "ruleSetsDeclarations.key:30")
	("alpha::seqGet" "alpha alpha::seqGet(Seq, int)" "seq.key:31")
	("seqLen" "int seqLen(Seq)" "seq.key:32")
	("seqIndexOf" "int seqIndexOf(Seq, any)" "seq.key:33")
	("seqGetOutside" "any seqGetOutside" "seq.key:34")
	("seqEmpty" "Seq seqEmpty" "seq.key:37")
	("seqSingleton" "Seq seqSingleton(any)" "seq.key:38")
	("seqConcat" "Seq seqConcat(Seq, Seq)" "seq.key:39")
	("seqSub" "Seq seqSub(Seq, int, int)" "seq.key:40")
	("seqReverse" "Seq seqReverse(Seq)" "seq.key:41")
	("seqDef{false,false,true}" "Seq seqDef{false,false,true}(int, int, any)" "seq.key:42")
	("seqSwap" "Seq seqSwap(Seq,int,int)" "seq.key:44")
	("seqRemove" "Seq seqRemove(Seq,int)" "seq.key:45")
	("seqNPermInv" "Seq seqNPermInv(Seq)" "seq.key:46")
	("array2seq" "Seq array2seq(Heap,Object)" "seq.key:48")
	("values" "Seq values" "seq.key:52")
	("seq_def_workaround" "Seq seq_def_workaround(Heap, int, int, Object)" "seq.key:55")
	("seq_def_workaround2" "Seq seq_def_workaround2(Heap, int, int, Object, LocSet)" "seq.key:56")
	("pair" "Pair pair(any, any)" "wellfound.key:36")
	("first" "any first(Pair)" "wellfound.key:37")
	("second" "any second(Pair)" "wellfound.key:38")	
	;;predicates 
	("clStartsWith" "clStartsWith(Seq,Seq)" "charListHeader.key:15")
	("clEndsWith" "clEndsWith(Seq,Seq)" "charListHeader.key:16")
	("clContains" "clContains(Seq,Seq)" "charListHeader.key:17")
	("wellFormed" "wellFormed(Heap)" "heap.key:41")
	("arrayStoreValid" "arrayStoreValid(any, any)" "heap.key:42")
	("nonNull" "nonNull(Heap,Object,int)" "heap.key:43")
	("wellOrderLeqInt" "wellOrderLeqInt(int, int)" "ifThenElseRules.key:36")
	("newObjectsIsomorphic" "newObjectsIsomorphic(Seq, Heap, Seq, Heap)" "infFlow.key:6")
	("newOnHeap" "newOnHeap(Heap, Seq)" "infFlow.key:7")
	("sameTypes" "sameTypes(Seq, Seq)" "infFlow.key:8")
	("sameType" "sameType(any, any)" "infFlow.key:9")
	("objectsIsomorphic" "objectsIsomorphic(Seq, Seq, Seq, Seq)" "infFlow.key:10")
	("objectIsomorphic" "objectIsomorphic(Seq, Object, Seq, Object)" "infFlow.key:11")
	("leq" "leq(int, int)" "integerHeader.key:210")
	("lt" "lt(int, int)" "integerHeader.key:211")
	("geq" "geq(int, int)" "integerHeader.key:212")
	("gt" "gt(int, int)" "integerHeader.key:213")
	("inByte" "inByte(int)" "integerHeader.key:222")
	("inChar" "inChar(int)" "integerHeader.key:223")
	("inInt" "inInt(int)" "integerHeader.key:224")
	("inLong" "inLong(int)" "integerHeader.key:225")
	("inShort" "inShort(int)" "integerHeader.key:226")
	("elementOf" "elementOf(Object, Field, LocSet)" "locSets.key:28")
	("subset" "subset(LocSet, LocSet)" "locSets.key:29")
	("disjoint" "disjoint(LocSet, LocSet)" "locSets.key:30")
	("createdInHeap" "createdInHeap(LocSet, Heap)" "locSets.key:31")
	("inDomain" "inDomain(Map, any)" "map.key:39")
	("inDomainImpliesCreated" "inDomainImpliesCreated(Map)" "map.key:40")
	("isFinite" "isFinite(Map)" "mapSize.key:29")
	("checkPermissionOwner" "checkPermissionOwner(Object,int,PermissionOwnerList)" "permission.key:54")
	("readPermissionObject" "readPermissionObject(Object, Permission)" "permission.key:57")
	("writePermissionObject" "writePermissionObject(Object, Permission)" "permission.key:58")
	("readPermission" "readPermission(Permission)" "permission.key:59")
	("writePermission" "writePermission(Permission)" "permission.key:60")
	("readPermissionOwe" "readPermissionOwe(Object, Object, Permission)" "permission.key:61")
	("readPermissionOwe2" "readPermissionOwe2(Object, Object, Permission)" "permission.key:62")
	("twoPermissions" "twoPermissions(Object,Object,Permission)" "permission.key:66")
	("nonEmptyPermission" "nonEmptyPermission(Permission)" "permission.key:67")
	("permissionsFor" "permissionsFor(Heap,Heap)" "permission.key:69")
	("acc" "acc(Heap, LocSet, Object, Object)" "reach.key:3")
	("reach" "reach(Heap, LocSet, Object, Object, int)" "reach.key:4")
	("match" "match(RegEx, Seq)" "regExHeader.key:16")
	("seqPerm" "seqPerm(Seq,Seq)" "seq.key:25")
	("seqNPerm" "seqNPerm(Seq)" "seq.key:26")
	("prec" "prec(any, any)" "wellfound.key:42")
	("measuredBy" "measuredBy(any)" "wellfound.key:43")
	("measuredByEmpty" "measuredByEmpty" "wellfound.key:44")
	("measuredByCheck" "measuredByCheck(any)" "wellfound.key:45")))

(defun company-key--make-candidate (item)
  (let ((a (first item)) (b (second item)) (c (third item)))    
    (propertize a 'definition b 'source c)))


(defun company-key-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-key-backend))
    (prefix (and (eq major-mode 'key-mode)
	     (company-grab-symbol)))
    
    (candidates
     (mapcar #'company-key--make-candidate
	     (cl-remove-if-not
	      (lambda (c) (string-prefix-p arg (first c)))
	      key-mode--completions)))

    (annotation (format " [%s]"
			(get-text-property 0 'definition arg)))
    
    (meta (format "Definition: %s from %s"
		  (get-text-property 0 'definition arg)
		  (get-text-property 0 'source arg)))))

(defun key-mode--company-support ()
  (add-to-list 'company-backends 'company-key-backend))


(eval-after-load 'company #'key-mode--company-support)


(provide 'key-mode)
;;; key-mode.el ends here
