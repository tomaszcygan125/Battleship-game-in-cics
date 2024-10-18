       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. Z02332.                                              
      ***************************************************************** 
      *                         "BATTLESHIP GAME"                       
      *                                                                 
      *                                                                 
      * THIS APPLICATION WILL ALLOW USER TO PLAY IN ONE OUT OF TWO MODES
      *                                                                 
      *  1. SINGLE PLAYER (WITH COMPUTER)                               
      *                                                                 
      *  2. MULTIPLAYER(WITH OTHER PLAYER)                              
      *                                                                 
      *                                                                 
      * MULTIPLAYER WILL BE ONLY POSSIBLE IF THERE WILL BE OTHER PLAYER 
      * SEARCHING FOR THE GAME IN THE SAME TIME                         
      *  IF THOSE TWO PLAYERS WONT FIND EACH OTHER THEN PROPER MESSAGE  
      * WILL BE DISPLAYED                                               
      *                                                                 
      *                                                                 
      *                                                                 
      * THERE ARE 4 TYPES OF SHIPS:                                     
      *                                                                 
      *  THERE CAN BE ONLY 1 SHIP OF LENGHT OF 5 FIELDS                 
      *  THERE CAN BE ONLY 2 SHIP OF LENGHT OF 4 FIELDS                 
      *  THERE CAN BE ONLY 3 SHIP OF LENGHT OF 3 FIELDS                 
      *  THERE CAN BE ONLY 4 SHIP OF LENGHT OF 2 FIELDS                 
      *                                                                 
      * A SHIP IS A LINE (HORIZONTAL OR VERTICAL) OF 'S' CHARACTERS     
      *                                                                 
      *                                                                 
      * PROGRAM WILL VALIDATE IF USER PROVIDED VALID SHIPS (AT THE      
      * START OF THE GAME ) AND WILL ALSO GENEREATE A SHIP BOARD        
      * IF THIS IS SINGLE PLAYER MODE ( IT WILL BE COMPUTER'S BOARD)    
      *                                                                 
      *                                                                 
      * USER WILL BE ABLE TO PLACE A CURSOR OVER ENEMY'S BOARD AND      
      * PRESS ENTER. THEN PROGRAM WILL GET CUROSR POSITION AND IF       
      * THIS POSITION WAS OVER ENEMY'S BOARD PROGRAM WILL VALIDATE      
      * IF SHOOT RESULT IN HIT OR MISS                                  
      *                                                                 
      * AS LONG AS USER HITS SOMETHING IT IS HIS TURN                   
      * IF HE MISSES THEN TURN GOES TO THE ENEMY (PLAYER OR COMPUTER)   
      *                                                                 
      *                                                                 
      * COMPUTER WILL MAKE HIS MOVE (OR MANY MOVES ) AND AFTER THAT     
      * CONTROL WILL GO BACK TO THE USER .                              
      *                                                                 
      * IF PROGRAM IS IN MULTIPLAYER MODE THEN PLAYER THAT LOSES CONTROL
      * WILL BE IN THE LOOP OF CHECKING IF THIS IS HIS TURN ( IF        
      * TURN RETURNED TO HIM) IF TURN WON'T CHANGE IN A GIVEN TIME THEN 
      * GAME WILL END DUE TO INACTIVITY OF THE USER                     
      *                                                                 
      * GAME END WHEN ALL SHIPS  OF ENEMY OR USER'S BOARD  ARE          
      * DESTROYED                                                       
      ***************************************************************** 
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
           COPY DFHAID.                                                 
           COPY ZZMP0234.                                               
           COPY ZZMP0235.                                               
           COPY ZZMP0236.                                               
           COPY DFHBMSCA.                                               
           EXEC SQL INCLUDE SQLCA   END-EXEC.                           
           EXEC SQL INCLUDE T01TABS END-EXEC.                           
           EXEC SQL INCLUDE T02TABS END-EXEC.                           
           EXEC SQL INCLUDE T03TABS END-EXEC.                           
           EXEC SQL INCLUDE T04TABS END-EXEC.                           
                                                                        
                                                                        
                                                                        
       01 CT-CONSTANTS.                                                 
           05 CT-MAXIMAL-VALUE               PIC S9(4) COMP  VALUE 81.  
           05 CT-MINIMAL-VALUE               PIC S9(4) COMP  VALUE 1.   
           05 CT-ERROR-MESSAGE               PIC X(50) VALUE            
            ' END OF TRANSACTION ' .                                    
           05 CT-MISSED-SHOT-SYMBOL          PIC X     VALUE 'O'.       
           05 CT-HIT-SHOT-SYMBOL             PIC X     VALUE 'X'.       
           05 CT-DESTROYED-SHIP-SYMBOL       PIC X     VALUE 'Z'.       
           05 CT-DISCONECTED-USER-SYMBOL.                               
               49 CT-DISCONNECTED-SYMBOL-LEN PIC S9(4) COMP  VALUE 6.   
               49 CT-DISCONNECTED-SYMBOL-TEXT PIC X(6) VALUE 'XXXXXX'.  
           05 CT-USER-WIN-SYMBOL.                                       
               49 CT-USER-WIN-SYMBOL-LEN     PIC S9(4) COMP  VALUE 6.   
               49 CT-USER-WIN-SYMBOL-TEXT    PIC X(6)  VALUE 'AAAAAA'.  
           05 CT-ENEMY-INACTIVE-SYMBOL.                                 
               49 CT-INACTIVE-SYMBOL-LEN     PIC S9(4) COMP  VALUE 6.   
               49 CT-INACTIVE-SYMBOL-TEXT    PIC X(6)  VALUE 'BBBBBB'.  
           05 WS-ERROR-MESSAGE             PIC X(50) VALUE 'ERROR EXIT'.
           05 CT-FIRST-FIELD-ON-ENEMY-BOARD  PIC S9(4) COMP  VALUE 360. 
           05 CT-WIDTH-OF-THE-SCREEN         PIC S9(4) COMP  VALUE 80.  
           05 CT-LAST-FIELD-ON-ENEMY-BOARD   PIC S9(4) COMP VALUE 1080. 
           05 CT-MAXIMAL-LOOP-COUNTER        PIC S9(4) COMP  VALUE 2500.
           05 CT-MAXIMAL-HEIGHT-OF-BORAD     PIC S9(4) COMP VALUE 10.   
           05 CT-MAXIMAL-WIDTH-OF-BOARD      PIC S9(4) COMP VALUE 10.   
           05 CT-TAKEN-SPOT                  PIC X VALUE 'X'.           
           05 CT-SHIP-FIELD                  PIC X VALUE 'S'.           
           05 CT-MAXIMAL-NUMBER-OF-CYCLES    PIC S9(4) COMP VALUE 5.    
           05 CT-MAX-FAILED-ITER-NUMBER      PIC S9(4) COMP VALUE 500.  
           05 CT-MAXIMAL-NUMBER-OF-SHOTS     PIC S9(4) COMP VALUE 100.  
       01 WS-DB2-ERROR.                                                 
           10 SW-SQLCODE                     PIC    S9(5).              
               88 SO-SQLCODE-OK              VALUE  000   100.          
               88 SO-SQLCODE-NORMAL          VALUE  000.                
               88 SO-SQLCODE-NOT-FOUND       VALUE  100.                
               88 SO-SQLCODE-NOT-UNIQUE      VALUE  -803.               
           10 WS-SQLERRMC                    PIC    X(70).              
           10 SQLCODE-FORMAT                 PIC    -(5).               
           10 SW-STATEMENT-ID                PIC    X(4).               
               88 SO-7001-PARA               VALUE  '7001'.       
               88 SO-7002-PARA               VALUE  '7002'.       
               88 SO-7003-PARA               VALUE  '7003'.       
               88 SO-7004-PARA               VALUE  '7004'.       
               88 SO-7005-PARA               VALUE  '7005'.       
               88 SO-7006-PARA               VALUE  '7006'.       
               88 SO-7007-PARA               VALUE  '7007'.       
               88 SO-7008-PARA               VALUE  '7008'.       
               88 SO-7009-PARA               VALUE  '7009'.       
               88 SO-7010-PARA               VALUE  '7010'.       
               88 SO-7011-PARA               VALUE  '7011'.       
               88 SO-7012-PARA               VALUE  '7012'.       
               88 SO-7013-PARA               VALUE  '7013'.       
               88 SO-7014-PARA               VALUE  '7014'.       
               88 SO-7015-PARA               VALUE  '7015'.       
               88 SO-7016-PARA               VALUE  '7016'.       
               88 SO-7017-PARA               VALUE  '7017'.       
               88 SO-7018-PARA               VALUE  '7018'.       
               88 SO-7019-PARA               VALUE  '7019'.       
               88 SO-7020-PARA               VALUE  '7020'.       
               88 SO-7021-PARA               VALUE  '7021'.       
               88 SO-7022-PARA               VALUE  '7022'.       
               88 SO-7023-PARA               VALUE  '7023'.       
               88 SO-7024-PARA               VALUE  '7024'.       
               88 SO-7025-PARA               VALUE  '7025'.       
               88 SO-7026-PARA               VALUE  '7026'.       
                                                                  
       01 SW-SWITCHES.                                            
           05 SW-WHAT-TYPE-OF-END            PIC X.               
              88 SO-FINAL-WITH-COMMAREA      VALUE '1'.           
              88 SO-FINAL-TERMINATION        VALUE '2'.           
              88 SO-TERMINATION-WITHOUT-MESS VALUE '3'.           
              88 SO-FINAL-FIRST-TIME         VALUE '4'.           
           05 SW-IF-FIRST-TIME               PIC X.               
              88 SO-FIRST-TIME               VALUE '1'.           
              88 SO-NOT-FIRST-TIME           VALUE '2'.           
           05 SW-IF-COMPUTER-MAP-GENERATED   PIC X.                     
              88 SO-COMPUTER-MAP-WAS-GENERATED VALUE '1'.               
              88 SO-TRY-GENERATE-MAP-AGAIN    VALUE '2'.                
           05 SW-IF-COMPUTER-MAP-INVALID     PIC X.                     
              88 SO-COMPUTER-MAP-IS-VALID    VALUE '1'.                 
              88 SO-INVALID-COMPUTER-MAP     VALUE '2'.                 
           05 SW-IF-WE-SHOULD-WAIT           PIC X.                     
              88 SO-WAIT-FOR-OPPONENT        VALUE '1'.                 
              88 SO-DONT-WAIT-FOR-OPPONENT   VALUE '2'.                 
           05 SW-IF-NOT-PLACED-FOUND         PIC X.                     
              88 SO-NOT-PLACED-NOT-FOUND     VALUE '1'.                 
              88 SO-NOT-PLACED-FOUND         VALUE '2'.                 
           05 SW-IF-ENEMY-IS-ACTIVE          PIC X.                     
              88 SO-ENEMY-IS-INACTIVE        VALUE '1'.                 
              88 SO-ENEMY-IS-ACTIVE          VALUE '2'.                 
           05 SW-IF-PLAYER-WAS-INACTIVE      PIC X.                     
              88 SO-PLAYER-WAS-NOT-INACTIVE  VALUE '1'.                 
              88 SO-PLAYER-WAS-INACTIVE      VALUE '2'.                 
           05 SW-IF-ENEMY-WONT-THE-GAME      PIC X.                     
              88 SO-ENEMY-WON-THE-GAME       VALUE '1'.                 
              88 SO-ENEMY-DINDNT-WIN-THE-GAME VALUE '2'.                
           05 SW-IF-DISPLAY-ERROR            PIC X.                     
              88 SO-DISPLAY-ERROR            VALUE '1'.                 
              88 SO-NOT-DISPLAY-ERROR        VALUE '2'.                 
           05 SW-IF-PLAYER-WIN-OR-NOT        PIC X.                     
              88 SO-THIS-PLAYER-WIN          VALUE '1'.                 
              88 SO-THIS-PLAYER-DIDNT-WIN    VALUE '2'.                 
           05 SW-WHAT-TYPE-OF-FLAG           PIC X.                     
              88 SO-ACTIVE-FLAG              VALUE 'A'.                 
              88 SO-NOT-ACTIVE-FLAG          VALUE 'B'.                 
           05 SW-IF-RECORD-EXIST             PIC X.                     
              88 SO-RECORD-DONT-EXIST        VALUE 'A'.                 
              88 SO-RECORD-EXIST             VALUE 'B'.                 
           05 SW-SIGLEPLAYER-FLAG            PIC X.                     
              88 SO-SINGLEPLAYER-CHOSEN      VALUE 'X'.                 
              88 SO-SINGLEPLAYER-EMPTY       VALUE LOW-VALUES SPACE '_'.
           05 SW-MULTIPLAYER-FLAG            PIC X.                     
              88 SO-MULTIPLAYER-CHOSEN       VALUE 'X'.                 
              88 SO-MULTIPLAYER-EMPTY        VALUE LOW-VALUES SPACE '_'.
           05 SW-IF-5-FIELD-SHIP-PLACD       PIC X.                     
              88 SO-5-FIELD-PLACED           VALUE '1'.                 
              88 SO-5-FIELD-NOT-PLACED       VALUE '2'.                 
           05 SW-IF-4-FIELD-SHIP-PLACD       PIC X.                     
              88 SO-4-FIELD-PLACED           VALUE '1'.                 
              88 SO-4-FIELD-NOT-PLACED       VALUE '2'.                 
           05 SW-IF-3-FIELD-SHIP-PLACD       PIC X.                     
              88 SO-3-FIELD-PLACED           VALUE '1'.                 
              88 SO-3-FIELD-NOT-PLACED       VALUE '2'.                 
           05 SW-IF-2-FIELD-SHIP-PLACD       PIC X.                     
              88 SO-2-FIELD-PLACED           VALUE '1'.                 
              88 SO-2-FIELD-NOT-PLACED       VALUE '2'.                 
           05 SW-WHAT-PLAYER-BOARD-TO-GET    PIC X.                     
              88 SO-GET-THIS-USER-BOARD      VALUE '1'.                 
              88 SO-GET-ENEMY-BOARD          VALUE '2'.                 
           05 SW-IF-ALL-SHIPS-ARE-PLACED     PIC X.                     
              88 SO-ALL-SHIPS-ARE-PLACED     VALUE '1'.                 
              88 SO-NOT-ALL-SHIPS-ARE-PLACED VALUE '2'.                 
           05 SW-IF-SHIP-CAN-BE-PLACED       PIC X.                     
              88 SO-SHIP-CAN-BE-PLACED       VALUE '1'.                 
              88 SO-SHIP-CANT-BE-PLACED      VALUE '2'.                 
           05 SW-WHAT-TYPE-OF-ORIENTATION    PIC X.                     
              88 SO-VERTICAL-ORIENTATION     VALUE '1'.                 
              88 SO-HORIZONTAL-ORIENTATION   VALUE '2'.                 
           05 SW-IF-SHIP-WAS-PLACED          PIC X.                     
              88 SO-SHIP-WAS-NOT-PLACED      VALUE '1'.                 
              88 SO-SHIP-WAS-PLACED          VALUE '2'.                 
           05 SW-IF-SHIP-POSTION-VALID       PIC X.                     
              88 SO-SHIP-POSITION-INVALID    VALUE '1'.                 
              88 SO-SHIP-POSITION-VALID      VALUE '2'.                 
           05 SW-IF-VALID-USER-SHIPS         PIC X.                     
              88 SO-VALID-USER-SHIPS         VALUE '1'.                 
              88 SO-INVALID-USER-SHIPS       VALUE '2'.                 
           05 SW-IF-SHIP-IS-HORIZONTAL       PIC X.                
              88 SO-SHIP-IS-HORIZONTAL       VALUE '1'.            
              88 SO-SHIP-IS-NOT-HORIZONTAL   VALUE '2'.            
           05 SW-IF-SHIP-IS-VERTICAL         PIC X.                
              88 SO-SHIP-IS-NOT-VERTICAL     VALUE '1'.            
              88 SO-SHIP-IS-VERTICAL         VALUE '2'.            
           05 SW-IF-NOT-END-OF-SHIP          PIC X.                
              88 SO-NOT-END-OF-SHIP          VALUE '1'.            
              88 SO-END-OF-SHIP              VALUE '2'.            
           05 SW-IF-USER-IS-CONNECTED        PIC X.                
              88 SO-ENEMY-IS-DISCONECTED     VALUE '1'.            
              88 SO-ENEMY-IS-CONNECTED       VALUE '2'.            
           05 SW-WHO-WILL-GET-FIRST-TURN     PIC X.                
              88 SO-USER-STARTS              VALUE '1'.            
              88 SO-COMPUTER-STARTS          VALUE '2'.            
           05 SW-IF-SHIP-WAS-DESTROYED       PIC X.                
              88 SO-DESTROYED-SHIP           VALUE '1'.            
              88 SO-NOT-DISTROYED-SHIP       VALUE '2'.            
              88 SO-NO-INFO-IF-HIT-OR-DEAD   VALUE '3'.            
           05 SW-IF-VALID-POSITION-FOUND     PIC X.                
              88 SO-FIND-OTHER-POSITION      VALUE '1'.            
              88 SO-VALID-POSITION-WAS-FOUND VALUE '2'.            
           05 SW-IF-BREAK-OUT-OF-THE-LOOP    PIC X.                
              88 SO-BREAK-OUT-OF-LOOP        VALUE '1'.            
              88 SO-DONT-BREAK-OUT-OF-LOOP   VALUE '2'.            
           05 SW-IF-THE-TURN-CHANGES         PIC X.                
              88 SO-IT-IS-SAME-SIDE-TURN     VALUE '1'.            
              88 SO-TURN-CHANGES             VALUE '2'.            
           05 SW-IF-SHOOT-WAS-VALID          PIC X.                
              88 SO-SHOOT-POSITION-IS-INVALID VALUE '1'.           
              88 SO-SHOOT-POSITION-IS-VALID   VALUE '2'.           
           05 SW-IF-SHIP-ORIENTATION-FOUND    PIC X.               
              88 SO-2131-ORIENTATION-VALID    VALUE '1'.           
              88 SO-2131-ORIENTATION-INVALID  VALUE '2'.           
           05 SW-IF-GAME-CAN-CONTINUE2        PIC X.               
              88 SO-SERIOUS-GAME-ERROR        VALUE '1'.           
              88 SO-GAME-CAN-CONTINUE         VALUE '2'.              
           05 SW-TYPE-OF-SHOT                 PIC X.                  
              88 SO-DESTROYED-SHOT            VALUE '1'.              
              88 SO-MISSED-SHOT               VALUE '2'.              
              88 SO-HIT-SHOT                  VALUE '3'.              
           05 SW-IF-HIT-AT-LEFT               PIC X.                  
              88 SO-HIT-AT-LEFT               VALUE '1'.              
              88 SO-MISS-AT-LEFT              VALUE '3'.              
              88 SO-NOT-HIT-AT-LEFT           VALUE '2'.              
           05 SW-IF-HIT-AT-RIGHT              PIC X.                  
              88 SO-HIT-AT-RIGHT              VALUE '1'.              
              88 SO-MISS-AT-RIGHT             VALUE '3'.              
              88 SO-NOT-HIT-AT-RIGHT          VALUE '2'.              
           05 SW-IF-HIT-AT-TOP                PIC X.                  
              88 SO-HIT-AT-TOP                VALUE '1'.              
              88 SO-MISS-AT-TOP               VALUE '3'.              
              88 SO-NOT-HIT-AT-TOP            VALUE '2'.              
           05 SW-IF-HIT-AT-BOTTOM             PIC X.                  
              88 SO-HIT-AT-BOTTOM             VALUE '1'.              
              88 SO-MISS-AT-BOTTOM            VALUE '3'.              
              88 SO-NOT-HIT-AT-BOTTOM         VALUE '2'.              
           05 SW-IF-CONTINUATION-ALLOWED      PIC X.                  
              88 SO-CONTINUE-WITH-POSITION      VALUE '1'.            
              88 SO-NOT-CONTINUE-WITH-POSITION  VALUE '2'.            
           05 SW-IF-RIGHT-SHOT-POSSIBLE       PIC X.                  
              88 SO-RIGHT-SHOT-POSSIBLE       VALUE '1'.              
              88 SO-RIGHT-SHOT-IMPOSSIBLE     VALUE '2'.              
           05 SW-IF-LEFT-SHOT-POSSIBLE        PIC X.                  
              88 SO-LEFT-SHOT-POSSIBLE        VALUE '1'.              
              88 SO-LEFT-SHOT-IMPOSSIBLE      VALUE '2'.              
           05 SW-IF-TOP-SHOT-POSSIBLE         PIC X.                  
              88 SO-TOP-SHOT-POSSIBLE         VALUE '1'.              
              88 SO-TOP-SHOT-IMPOSSIBLE       VALUE '2'.              
           05 SW-IF-BOTTOM-SHOT-POSSIBLE      PIC X.                  
              88 SO-BOTTOM-SHOT-POSSIBLE      VALUE '1'.              
              88 SO-BOTTOM-SHOT-IMPOSSIBLE    VALUE '2'.              
           05 SW-IF-CHOSEN-SIDE-IS-VALID      PIC X.            
              88 SO-INVALID-SIDE              VALUE '1'.        
              88 SO-VALID-SIDE                VALUE '2'.        
           05 SW-IF-OPPOSITE-IS-VALID         PIC X.            
              88 SO-OPPOSITE-IS-VALID         VALUE '1'.        
              88 SO-OPPOSITE-IS-INVALID       VALUE '2'.        
           05 SW-WHICH-SIDE-SHOT               PIC X.           
              88 SO-SHOT-IN-OPPOSITE-DIRECTION VALUE '1'.       
              88 SO-SHOT-IN-LAST-HIT-DIRECTION VALUE '2'.       
           05 SW-IF-SIDE-IS-VALID-OR-NOT      PIC X.            
              88 SO-SIDE-IS-VALID             VALUE '1'.        
              88 SO-SIDE-IS-INVALID           VALUE '2'.        
           05 SW-IF-TURN-CHANGED-TO-COMPUTER  PIC X.            
              88 SO-TURN-NOT-CHANGED-TO-COMP  VALUE '1'.        
              88 SO-TURN-CHANGED-TO-COMPUTER  VALUE '2'.        
           05 SW-IF-USER-NICK-IS-VALID        PIC X.            
              88 SO-INVALID-NICK              VALUE '1'.        
              88 SO-VALID-NICK                VALUE '2'.        
           05 SW-WHAT-TYPE-OF-CHECK           PIC X.            
              88 SO-HORIZONTAL-CHECK          VALUE '1'.        
              88 SO-VERTICAL-CHECK            VALUE '2'.        
           05 SW-WHAT-TYPE-OF-CHECK2          PIC X.            
              88 SO-CHECK-HORIZONTAL-NEIGHBOR VALUE '1'.        
              88 SO-CHECK-VERICAL-NEIGHBOUR   VALUE '2'.        
           05 SW-IF-ENEMY-WAS-FOUND           PIC X.            
              88 SO-ENEMY-NOT-FOUND           VALUE '1'.        
              88 SO-ENEMY-FOUND               VALUE '2'.        
           05 SW-IF-ENEMY-WAS-FOUND           PIC X.            
              88 SO-OUR-TURN                  VALUE '1'.        
              88 SO-ENEMY-TOUR                VALUE '2'.        
           05 SW-IF-WE-HAVE-THE-SHIPS         PIC X.            
              88 SO-WE-DONT-HAVE-SHIPS        VALUE '1'.        
              88 SO-WE-HAVE-THE-SHIPS         VALUE '2'.        
           05 SW-IF-COMPUTER-HIT-SOMETHING    PIC X.            
              88 SO-COMPUTER-MISSED           VALUE '1'.        
              88 SO-COMPUTER-HIT-SOMETHING    VALUE '2'.        
              88 SO-COMPUTER-HIT-TAKEN-FIELD  VALUE '3'.                
           05 WS-RANDOM-VALUE                 PIC S9(4) COMP VALUE 0.   
              88 SO-RIGHT-SIDE-CHOSEN         VALUE 1.                  
              88 SO-LEFT-SIDE-CHOSEN          VALUE 2.                  
              88 SO-TOP-SIDE-CHOSEN           VALUE 3.                  
              88 SO-BOTTOM-SIDE-CHOSEN        VALUE 4.                  
           05 SW-WHAT-TYPE-OF-FIELD           PIC X.                    
              88 SO-HIT-FIELD                 VALUE 'X'.                
              88 SO-MISSED-FIELD              VALUE 'O'.                
              88 SO-EMPTY-FIELD               VALUE ' '.                
              88 SO-SHIP-FIELD                VALUE 'S'.                
              88 SO-DESTROYED-SHIP-FIELD      VALUE 'Z'.                
       01 WS-VARIABLES.                                                 
           05 WS-NON-PLACED-ITERATIONS        PIC S9(4) COMP VALUE 0.   
           05 WS-NUMBER-OF-CYCLES             PIC S9(4) COMP VALUE 0.   
           05 WS-ROW-ITERATOR                 PIC S9(4) COMP VALUE 0.   
           05 WS-COLUMN-ITERATOR              PIC S9(4) COMP VALUE 0.   
           05 WS-ROW-ITERATOR-FORMAT          PIC S9(4)  VALUE 0.       
           05 WS-COL-ITERATOR-FORMAT          PIC S9(4)  VALUE 0.       
           05 WS-POSITION-ITERATOR            PIC S9(4) COMP VALUE 0.   
           05 WS-SHIP-TYPES-ARRAY             OCCURS 5 TIMES.           
             10 WS-THIS-TYPE-SHIP-COUNTER     PIC S9(4) COMP .          
             10 WS-MAXIMAL-AMOUNT-OF-SHIPS    PIC S9(4) COMP.           
             10 SW-IF-SHIP-PLACED             PIC X.                    
                88 SO-THIS-TYPE-SHIP-PLACED   VALUE 'A'.                
                88 SO-THIS-TYPE-NOT-PLACED    VALUE 'B'.                
                                                                        
                                                                        
           05 WS-DB2-ENEMY-NICK.                                        
              49 WS-DB2-ENEMY-NICK-LEN      PIC S9(4) COMP.             
              49 WS-DB2-ENEMY-NICK-TEXT     PIC X(30).                  
           05 WS-DB2-USER-NICK.                                         
              49 WS-DB2-USER-NICK-LEN      PIC S9(4) COMP.              
              49 WS-DB2-USER-NICK-TEXT     PIC X(30).                   
           05 WS-HOW-MANY-LOOPS            PIC S9(4) COMP VALUE 0.      
           05 WS-USER-NAME.                                             
              49 WS-USER-NAME-LEN          PIC S9(4) COMP VALUE 0.     
              49 WS-USER-NAME-TEXT         PIC X(30) VALUE SPACE.      
           05 WS-DUMMY-VALUE               PIC X.                      
           05 WS-STRING-TEMP               PIC X(30) VALUE SPACE.      
           05 WS-USER-NICK                 PIC X(30) VALUE SPACE.      
           05 WS-USER-NICK-LEN             PIC S9(4) COMP VALUE 0.     
           05 WS-SPACE-COUNTER             PIC S9(4) COMP VALUE 0.     
           05 WS-ROW-OFFSET-FORMAT         PIC 9(10) VALUE 0.          
           05 WS-COLUMN-OFFSET-FORMAT      PIC 9(10) VALUE 0.          
           05 WS-ROW-OFFSET                PIC S9(4) COMP VALUE 0.     
           05 WS-COLUMN-OFFSET             PIC S9(4) COMP VALUE 0.     
           05 WS-OPPOSITE-ROW              PIC S9(4) COMP VALUE 0.     
           05 WS-OPPOSITE-COLUMN           PIC S9(4) COMP VALUE 0.     
           05 WS-MAIN-FIELD-ROW            PIC S9(4) COMP VALUE 0.     
           05 WS-MAIN-FIELD-COLUMN         PIC S9(4) COMP VALUE 0.     
           05 WS-ROW-CHECK-POS             PIC S9(4) COMP VALUE 0.     
           05 WS-COLUMN-CHECK-POS          PIC S9(4) COMP VALUE 0.     
           05 WS-OPTION-1                  PIC X.                      
           05 WS-OPTION-2                  PIC X.                      
           05 WS-FIELD-COUNTER             PIC S9(4) COMP VALUE 0.     
           05 WS-MAX-ROW                   PIC S9(4) COMP VALUE 0.     
           05 WS-TEMP-FIELD                PIC X.                      
           05 WS-HOW-MANY-ELEMENTS         PIC S9(4) COMP VALUE 0.     
           05 WS-5-FIELDS-SHIP-COUNTER     PIC S9(4) COMP VALUE 0.     
           05 WS-4-FIELDS-SHIP-COUNTER     PIC S9(4) COMP VALUE 0.     
           05 WS-3-FIELDS-SHIP-COUNTER     PIC S9(4) COMP VALUE 0.     
           05 WS-2-FIELDS-SHIP-COUNTER     PIC S9(4) COMP VALUE 0.     
           05 WS-TEMP-VALUE                PIC S9(8) COMP-3.           
           05 WS-TEMP-VALUE2               PIC  9(8) .                 
           05 WS-TIME-VALUE                PIC X(8) VALUE SPACE.       
           05 WS-FIRST-SEED                PIC S9(9) COMP VALUE 0.     
           05 WS-TEMP-NUMERIC              PIC S9(4) COMP VALUE 0.     
           05 WS-COLUMN-POSITION           PIC S9(4) COMP VALUE 0.     
           05 WS-ROW-POSITION              PIC S9(4) COMP VALUE 0.     
           05 WS-COLUMN-POSITION-TEMP      PIC S9(4) COMP VALUE 0.     
           05 WS-ROW-POSITION-TEMP         PIC S9(4) COMP VALUE 0.     
           05 WS-COLUMN-VALIDATION         PIC S9(4) COMP VALUE 0.   
           05 WS-ROW-VALIDATION            PIC S9(4) COMP VALUE 0.   
           05 WS-COLUMN-VALIDATION-TEMP    PIC S9(4) COMP VALUE 0.   
           05 WS-ROW-VALIDATION-TEMP       PIC S9(4) COMP VALUE 0.   
           05 WS-ROW-TEMP                  PIC S9(4) COMP VALUE 0.   
           05 WS-COLUMN-TEMP               PIC S9(4) COMP VALUE 0.   
           05 WS-TEMP-ROW                  PIC S9(4) COMP VALUE 0.   
           05 WS-TEMP-COLUMN               PIC S9(4) COMP VALUE 0.   
                                                                     
           05 WS-FIELD-NUMBER              PIC S9(4) COMP VALUE 0.   
           05 WS-RANDOM-ORIENTATION        PIC 9V9(2) VALUE 0.       
           05 WS-COMMMAREA                 PIC X(81).                
           05 WS-ITER1                     PIC S9(4) COMP VALUE 0.   
           05 WS-ITER2                     PIC S9(4) COMP VALUE 0.   
           05 WS-ITER3                     PIC S9(4) COMP VALUE 0.   
           05 WS-ITER4                     PIC S9(4) COMP VALUE 0.   
           05 WS-ITER5                     PIC S9(4) COMP VALUE 0.   
           05 WS-ITER6                     PIC S9(4) COMP VALUE 0.   
           05 WS-ITER7                     PIC S9(4) COMP VALUE 0.   
           05 WS-ITER19                    PIC S9(4) COMP VALUE 0.   
           05 WS-TYPE-OF-SHIP              PIC S9(4) COMP VALUE 0.   
           05 WS-ITER-X                    PIC S9(4) COMP VALUE 0.   
           05 WS-ITER-Y                    PIC S9(4) COMP VALUE 0.   
           05 WS-ITER-ROW                  PIC S9(4) COMP VALUE 0.   
           05 WS-ITER-COLUMN               PIC S9(4) COMP VALUE 0.   
           05 WS-ITER-ROW-TEMP             PIC S9(4) COMP VALUE 0.   
           05 WS-ITER-COLUMN-TEMP          PIC S9(4) COMP VALUE 0.   
           05 WS-ITERX                     PIC S9(4) COMP VALUE 0.   
           05 WS-EIBRESP-FORMAT            PIC X(30).                
                                                                     
           05 WS-RANDOM-VALUE-TEMP         PIC 99 VALUE 0.           
           05 WS-RANDOM-VALUE-TEXT.                                  
              10 WS-RANDOM-VALUE-FISRT-NUMBER  PIC X VALUE SPACE.    
              10 WS-RANDOM-VALUE-SECOND-NUMBER PIC X VALUE SPACE.    
           05 WS-PREVIOUS-SEED             PIC S9(4) COMP VALUE 0.   
           05 WS-ENEMY-BOARD-COUNTER       PIC S9(4) COMP VALUE 0.   
           05 WS-USER-BOARD-COUNTER        PIC S9(4) COMP VALUE 0.    
                                                                      
           05 WS-AMOUNT-OF-SHIPS           PIC S9(4) COMP VALUE 10.   
           05 WS-SHIP-COUNTER              PIC S9(4) COMP VALUE 0.    
           05 WS-SCREEN-TABLE OCCURS 10 TIMES.                        
               10 WS-SCREEN-LINE PIC X(10).                           
           05 WS-SCREEN-TABLE2 OCCURS 10 TIMES.                       
               10 WS-SCREEN-LINE2 PIC X(10).                          
           05 WS-PROGRAM-ARRAY OCCURS 10 TIMES.                       
               10 WS-PROGRAM-ARRAY-LINE PIC X(10).                    
           05 WS-PROGRAM-ARRAY-TEMP OCCURS 10 TIMES.                  
               10 WS-PROGRAM-ARRAY-LINE-TEMP PIC X(10).               
                                                                      
       01 WS-COMMAREA.                                                
           05 WS-MINE-NICK                    PIC X(30).              
           05 WS-ENEMY-NICK                   PIC X(30).              
           05 SW-IF-USER-PROVIDED-NICK        PIC X.                  
              88 SO-USER-DIDNT-PROVIDE-NICK   VALUE '1'.              
              88 SO-USER-PROVIDED-NICK        VALUE '2'.              
           05 SW-IF-USER-SHOT-IS-VALID        PIC X.                  
              88 SO-USER-SHOT-POSITION-VALID  VALUE '1'.              
              88 SO-USER-SHOT-POSITION-INVALID VALUE '2'.             
           05 SW-IF-USER-PLACED-SHIPS         PIC X.                  
              88 SO-USER-DIDNT-PLACED-SHIPS   VALUE '1'.              
              88 SO-USER-PLACED-SHIPS         VALUE '2'.              
           05 SW-IF-USER-MADE-A-CHOICE        PIC X.                  
              88 SO-USER-CHOOSE-GAME-MODE     VALUE '1'.              
              88 SO-USER-DIDNT-CHOOSE-THE-MODE VALUE '2'.             
           05 SW-PROGRAM-MODE                 PIC X.                  
              88 SO-MODE-SINGLE-PLAYER        VALUE '1'.              
              88 SO-MODE-MULTIPLAYER          VALUE '2'.              
           05 SW-WHOSE-TURN                   PIC X.                  
              88 SO-IT-IS-USERS-TURN          VALUE '1'.              
              88 SO-IT-IS-COMPUTERS-TURN      VALUE '2'.              
           05 SW-IF-GAME-CAN-CONTINUE         PIC X.                  
              88 SO-GAME-CAN-BE-CONTINUED     VALUE '1'.              
              88 SO-GAME-SHOULD-END           VALUE '2'.                
           05 SW-WHO-IS-THE-WINNER            PIC X(11).                
              88 SO-COMPUTER-WON              VALUE 'COMPUTER'.         
              88 SO-USER-WON                  VALUE 'USER - YOU'.       
           05 SW-IF-ENEMY-MADE-A-RECORD       PIC X.                    
              88 SO-OUR-ENEMY-MADE-RECORD     VALUE '1'.                
              88 SO-ENEMY-DIDNT-MAKE-RECORD   VALUE '2'.                
           05 SW-WHAT-COMPUTER-SHOULD-DO      PIC X.                    
              88 SO-RANDOM-SHOT               VALUE '1'.                
              88 SO-BOTTOM-SHOT               VALUE '2'.                
              88 SO-TOP-SHOT                  VALUE '3'.                
              88 SO-LEFT-SHOT                 VALUE '4'.                
              88 SO-RIGHT-SHOT                VALUE '5'.                
           05 WS-LAST-EIBCPOSN                PIC S9(4) COMP.           
           05 WS-COMPUTER-LAST-SHOT-POS.                                
              10 WS-COMPUTER-LAST-ROW-POS     PIC S9(4) COMP.           
              10 WS-COMPUTER-LAST-COLUMN-POS  PIC S9(4) COMP.           
           05 WS-COMPUTER-BOARD               OCCURS 10 TIMES.          
              10 WS-COMPUTER-BOARD-LINE       PIC X(10).                
           05 WS-USER-BOARD                   OCCURS 10 TIMES.          
              10 WS-USER-BOARD-LINE           PIC X(10).                
                                                                        
       LINKAGE SECTION.                                                 
       01 DFHCOMMAREA PIC X(286).                                       
       PROCEDURE DIVISION USING DFHCOMMAREA.                            
           DISPLAY '((((((((((((((((  Z02332 PERFORMED ))))))))'        
           DISPLAY 'TYP DZIALANIA: (PO STARCIE)  ' SW-PROGRAM-MODE      
           PERFORM 1000-INIT                                            
           PERFORM 2000-PROCESS                                         
           PERFORM 3000-FINAL                                           
           .                                                            
      ******************************************************************
      *                     1000-INIT                                   
      ******************************************************************
       1000-INIT.                                                       
           DISPLAY '1000-INIT PERFORMED'                                
           INITIALIZE WS-VARIABLES                                      
           PERFORM 1005-IGNORE-CICS                                     
           PERFORM 1010-CHECK-IF-FIRST-TIME                             
           .                                                            
      ******************************************************************
      *                       1005-IGNORE-CICS                          
      ******************************************************************
       1005-IGNORE-CICS.                                                
           DISPLAY '1005 IGNORE CICS PERFORMED   '                      
           EXEC CICS                                                    
            IGNORE CONDITION  ERROR                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   1010-CHECK-IF-FIRST-TIME                      
      * PARAGRAPH WILL CHECK IF TRANSACTION WAS INITIATED FOR THE       
      * FIRST TIME OR IT WAS INITIATED BEFORE                           
      *                                                                 
      * DEPENDING ON THAT FACT PARAGRAPH WILL SET THE FLAGS             
      *                                                                 
      ******************************************************************
       1010-CHECK-IF-FIRST-TIME.                                        
           DISPLAY '1010 CHECK IF FIRST TIME PERFORMED '                
           SET SO-NOT-DISPLAY-ERROR          TO TRUE                    
                                                                        
           IF EIBCALEN = 0 THEN                                         
           DISPLAY '1010  FIRST TIME '                                  
             PERFORM 1015-INITIALIZE-PROGRAM-DATA                       
             PERFORM 1020-INITIALIZE-FIRST-MAP                          
           ELSE                                                         
             DISPLAY '1010 NOT A FIRST TIME '                           
             SET  SO-NOT-FIRST-TIME          TO TRUE                    
             MOVE DFHCOMMAREA                TO WS-COMMAREA             
             SET  SO-FINAL-WITH-COMMAREA     TO TRUE                    
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  1015-INITIALIZE-PROGRAM-DATA                   
      ******************************************************************
       1015-INITIALIZE-PROGRAM-DATA.                                    
           DISPLAY '1015 INITIALIZE-FLAGS PERFORMED '                   
           SET SO-USER-SHOT-POSITION-INVALID TO TRUE                    
           SET SO-USER-DIDNT-CHOOSE-THE-MODE TO TRUE                    
           SET SO-USER-DIDNT-PLACED-SHIPS    TO TRUE                    
           SET SO-FIRST-TIME                 TO TRUE                    
           SET SO-RANDOM-SHOT                TO TRUE                    
           SET SO-TURN-NOT-CHANGED-TO-COMP   TO TRUE                    
           SET SO-ENEMY-DIDNT-MAKE-RECORD    TO TRUE                    
           SET SO-ENEMY-IS-CONNECTED         TO TRUE                    
           SET SO-GAME-CAN-BE-CONTINUED      TO TRUE                    
           SET SO-ENEMY-DINDNT-WIN-THE-GAME  TO TRUE                    
           SET SO-THIS-PLAYER-DIDNT-WIN      TO TRUE                    
           SET SO-PLAYER-WAS-NOT-INACTIVE    TO TRUE                    
           SET SO-ENEMY-IS-ACTIVE            TO TRUE                    
           SET SO-FINAL-WITH-COMMAREA        TO TRUE                    
           MOVE 0                            TO WS-LAST-EIBCPOSN        
           .                                                            
      ******************************************************************
      *                  1020-INITIALIZE-FIRST-MAP                      
      ******************************************************************
       1020-INITIALIZE-FIRST-MAP.                                       
           DISPLAY '1020-INITIALIZE-FIRST-MAP PERFORMED'                
           INITIALIZE       MP0235O                                     
           MOVE LOW-VALUES TO  CHOIC1A                                  
           MOVE LOW-VALUES TO  CHOIC2A                                  
           DISPLAY 'CHOIC1A: '  CHOIC1A                                 
           DISPLAY 'CHOIC2A: '  CHOIC2A                                 
           .                                                            
      ******************************************************************
      *                     2000-PROCESS                                
      * THIS IS THE MAIN PROCESS PARAGRAPH OF THE APPLICATION           
      * HERE WE CAN PERFORM 2 TASKS:                                    
      * 1. WE CAN JUST SEND THE CHOICE MAP WHERE USER WILL HAVE         
      * TO PROVIDE HIS CHOICE ( IF HE WANTS TO PLAY IN SINGLEPLAYER OR  
      * MULTIPLAYER                                                     
      *                                                                 
      * 2. IF THIS IS NOT THE FIRST TIME PROGRAM RUNS THEN WE WILL      
      * CHECK WHAT KEY WAS PRESSED BY THE USER AND BASED ON THAT        
      * SPECIFIC ACTIONS WILL BE TAKEN                                  
      *                                                                 
      ******************************************************************
       2000-PROCESS.                                                    
           DISPLAY '2000-PROCESS PERFORMED '                            
                                                                        
           IF SO-FIRST-TIME THEN                                        
              DISPLAY '2000-PROCES SO-FIRST-TIME'                       
                                                                        
              PERFORM 2101-SEND-THE-CHOICE-MAP                          
           ELSE                                                         
      * IF USER PROVIDED HIS CHOICE                                     
      * (SINGLE OR MULTIPLAYER )                                        
               DISPLAY '2000 USER MADE A CHOICE '                       
               SET SO-FINAL-WITH-COMMAREA TO TRUE                       
               DISPLAY 'SW-WHAT-TYPE-OF-END ' SW-WHAT-TYPE-OF-END       
      * PARAGRAPH BELOW WILL EVALUATE THROUGH ALL POSSIBL KEYS          
      * THAT USER COULD PRESS, PARAGRAPH WILL MAKE ACTIONS              
      * DEPENDING ON WHAT USER HAVE PRESSED                             
               PERFORM 2002-MAKE-ACTION-BASED-ON-KEY                    
               MOVE WS-COMMAREA TO DFHCOMMAREA                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2002-MAKE-ACTION-BASED-ON-KEY                 
      *                                                                 
      * THIS PARAGRAPH EVALUATES THROUGH ALL KEYS THAT HAVE MEANING IN  
      * THE APPLICATION ( THEY HAVE ACTION ASSIGNED)                    
      *                                                                 
      * HERE WE WILL ALSO CHECK FOR SITUATION WHERE USER WILL           
      * PRESS NO ACTION KEY, ( THEN HE WILL SEE AN ERROR MESSAGE)       
      *                                                                 
      * PRESSING F3 ISSUES AN END OF THE TRANSACTION, IT WILL JUST      
      * FORCE PROGRAM TO END                                            
      *                                                                 
      *                                                                 
      * WHEN USER PRESSED ENTER THERE CAN BE MULTIPLE SCENARIOS         
      * WHERE HE IS AND WHAT HE IS TRYING TO DO                         
      * THIS SCENARIOS WILL BE:                                         
      *                                                                 
      *   1. USER CHOOSE SINGPLE/MULTIPLAYER AND PRESSED ENTER          
      *   2. (MULTIPLYAER) USER PROVIDED NICK AND PRESSED ENTER         
      *   3. SINGLE/MULTIPLAYER USER PROVIDED SHIPS AND PRESSED ENTER   
      *   4. SINGLE/MULTIPLAYER USER SHOOTS AT THE ENEMY (              
      *   USER PLACED CURSOR ON THE SCREEN AN THEN PRESSED ENTER)       
      *                                                                 
      *   ALL OF THE ACTIONS DESCRIBED ABOVE WILL RESULT IN             
      * PERFORMING 2304-PROCESS-WHEN-ENTER PARAGRAPH THAT WILL MAKE     
      *  FURTHER VALIDATION ABOUT IN WHAT SITUATION USER IS RIGHT       
      * NOW                                                             
      ******************************************************************
       2002-MAKE-ACTION-BASED-ON-KEY.                                   
           DISPLAY '2002-MAKE-ACTION-BASED-ON-KEY PERFORMED '           
           EVALUATE EIBAID                                              
      * PARAGRAPH WILL BE PERFORMED WHEN USER PRESSED ENTER             
           WHEN DFHENTER                                                
              DISPLAY ' 2002 ENTER PRESSED'                             
                                                                        
              PERFORM 2304-PROCESS-WHEN-ENTER                           
              DISPLAY 'AFTER 2304 (ENTER) '                             
           WHEN DFHPF3                                                  
              DISPLAY ' 2002 F3 PRESSED'                                
      * IF USER PRESSED F3 KEY THEN THE FLAG BELOW WILL BE              
      * MODIFIED AND PARAGRAPH 3000 WILL TERMINATE THE TRANSACTION      
              PERFORM 2305-PROCESS-WHEN-F3-PRESSED                      
           WHEN OTHER                                                   
              DISPLAY ' 2002 OTHER KEY PERSSED'                         
      * PARAGRAPH WILL BE PERFORMED WHEN USER PRESSED KEY THAT DONT     
      * HAVE ANY ACTION ASSIGNED ( ERROR MESSAGE WILL BE DISPLAYED)     
              PERFORM 2306-PROCESS-WHEN-OTHER-KEY                       
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    2003-RECEIVE-MAP-FROM-USER                   
      ******************************************************************
       2003-RECEIVE-MAP-FROM-USER.                                      
           DISPLAY '2003-RECEIVE-MAP-FROM-USER PERFORMED '              
           INITIALIZE    MP0234I                                        
           DISPLAY '2003 MP0234I: ' MP0234I                             
           EXEC CICS                                                    
             RECEIVE MAP('MP0234') MAPSET('MP0234')                     
             INTO(MP0234I)                                              
             NOHANDLE                                                   
           END-EXEC                                                     
                                                                        
           DISPLAY '2003 RECEIVE STATEMENT WAS EXECUTED '               
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2005-PREPERE-MAP-OF-COMPUTER                   
      *                                                                 
      * THIS PARAGRAPH WILL TRY TO GENERATE A COMPUTER MAP              
      * COMPUTER MAP WILL CONTAINS RANDOMLY PLACED SHIPS                
      * SHIPS NUMBER WILL BE THE SAME AS FOR THE USER SO:               
      *                                                                 
      *  THERE WILL BE ONE SHIP     THAT IS  5 FIELDS LONG              
      *  THERE WILL BE TWO SHIPS    THAT ARE 4 FIELDS LONG              
      *  THERE WILL BE THREE SHIPS  THAT ARE 3 FIELDS LONG              
      *  THERE WILL BE FOUR SHIPS   THAT ARE 2 FIELDS LONG              
      *                                                                 
      * BECAUSE OF THE FACT THAT THERE ARE RULES ABOUT HOW THE SHIPS    
      * SHOULD LOOK LIKE THERE COULD BE A SITUATION WHERE               
      * SHIP CANNOT BE PLACED BECAUSE THERE IS NO PLACE FOR HIM         
      *                                                                 
      * IN THAT SCENARIO ALL MAP THAT WAS GENERATED TO THAT MOMENT WILL 
      * BE RESETED AND WHOLE PROCESS OF GENERATEING THE MAP             
      * WILL BE INITIATED ONCE AGAIN                                    
      * THERE CAN BE ONLY 5 ITERATIONS OF THE CYCLE DESCRIBED           
      * ABOVE                                                           
      *                                                                 
      * CYCLE WILL BE REPATED WHEN THERE WAS MORE THAN 100 TRYIES OF    
      * FINDING THE CORRECT SPOT IF THIS DIDNT HAPPEN THEN PROGRAM      
      * WILL INITIATE NEXT CYCLE ( TO MAXIMUM 5 ITERATIONS )            
      *                                                                 
      *                                                                 
      * TO CREATE THIS LOGIC WE WILL USE 2 LOOPS IN THIS PARAGRAPH      
      *                                                                 
      * ONE WILL GO MAXIMUM OF 5 TIMES AND INNER ONE WILL GO            
      * MAXIMUM OF 2500 TIMES OR LESS IF THERE WAS MORE THAN 100        
      * TRIES OF PLACING A SHIP                                         
      *                                                                 
      *                                                                 
      * HERE I WILL DESCRIBE THE RULES OF PLACING SHIPS ON THE MAP      
      *                                                                 
      *    1. THERE IS A LIMIT OF SHIPS UNITS THAT CAN BE PLACED        
      *     ON THE MAP ( NUMBERS WERE PRESENTED ABOVE)                  
      *    2. ALL TYPES OF SHIPS HAS TO BE PLACED AND THIER NUMBER      
      *    HAS TO BE FULFILLED                                          
      *    3. SHIPS' FIELD CANT HAVE NEIGBHBOUR HORIZONTALLY OR         
      *     VERTICALLY ( SHIP CANT TOUCH OTHER SHIP)                    
      *    4. SHIPS CAN HAVE NEIGBOURS DIAGONALLY                       
      *    5. SHIPS CANOT BEND ( THEY ARE ALWAYS VERTICAL OR            
      *      HORIZONTAL  !STRIGHT! LINE                                 
                                                                        
      ******************************************************************
       2005-PREPERE-MAP-OF-COMPUTER.                                    
           DISPLAY '2005- PERFORMED'                                    
           SET  SO-TRY-GENERATE-MAP-AGAIN   TO TRUE                     
           SET  SO-NOT-ALL-SHIPS-ARE-PLACED TO TRUE                     
           MOVE 0                           TO WS-NUMBER-OF-CYCLES      
           DISPLAY '2005 WS-NUMBER-OF-CYCLES: ' WS-NUMBER-OF-CYCLES     
           DISPLAY 'SW-IF-COMPUTER-MAP-GENERATED  '                     
                        SW-IF-COMPUTER-MAP-GENERATED                    
           DISPLAY 'SW-IF-ALL-SHIPS-ARE-PLACED '                        
                         SW-IF-ALL-SHIPS-ARE-PLACED                     
           DISPLAY '2005 BEFORE UPPER LOOP'                             
      * OUTER LOOP WILL LIMIT NUMBER OF CYCLES THE MAP GENERATION       
      * PROCES CAN TAKE                                                 
      *                                                                 
      * EACH CYCLE STARTS WHEN PROGRAM COULD NOT PLACE ANY VALID        
      * SHIP FOR LONGER THAN A GIVEN CONSTANT                           
      *                                                                 
      *                                                                 
      *  THEN CYCLE WILL START AND ALL SHIPS PLACED TO THAT POINT       
      * WILL BE RESET                                                   
           PERFORM UNTIL SO-COMPUTER-MAP-WAS-GENERATED                  
               OR SO-ALL-SHIPS-ARE-PLACED  OR WS-NUMBER-OF-CYCLES >     
                                         CT-MAXIMAL-NUMBER-OF-CYCLES    
                                                                        
              DISPLAY '2005 IN OUTER LOOP '                             
              DISPLAY '2005 NUMBER OF CYCLES: ' WS-NUMBER-OF-CYCLES     
                                                                        
              PERFORM  2331-INITIALIZE-SHIPS-VARS                       
              ADD 1 TO WS-NUMBER-OF-CYCLES                              
      * PARAGRAPH WILL BE USING RANDOM FUNCTION SO WE WILL HAVE         
      * TO GET  A FIRST SEED FOR IT                                     
              PERFORM 2007-GET-FIRST-SEED                               
                                                                        
              DISPLAY '2005 NUMBER OF CYCLES: ' WS-NUMBER-OF-CYCLES     
              DISPLAY 'LOOP START'                                      
              PERFORM UNTIL SO-ALL-SHIPS-ARE-PLACED OR                  
                            SO-INVALID-COMPUTER-MAP                     
                                                                        
                 DISPLAY '2005 IN LOWER LOOP '                          
                 PERFORM 2006-GET-RANDOM-POSITION                       
                 PERFORM 2008-GET-RANDOM-ORIENTATION                    
                 PERFORM 2009-PREPARE-SHIP-DATA                         
                                                                        
                 IF SO-SHIP-CAN-BE-PLACED THEN                          
                      PERFORM 2014-PLACE-THE-SHIP                       
                 END-IF                                                 
      * PARAGRAPH WILL COUNT NUMBER OF ITERATIONS THAT                  
      * DIDNT PLACE ANY SHIP                                            
      *                                                                 
                 PERFORM 2329-COUNT-FAILED-LOOP-ITERS                   
                                                                        
                 IF WS-NON-PLACED-ITERATIONS > CT-MAX-FAILED-ITER-NUMBER
                        SET SO-TRY-GENERATE-MAP-AGAIN TO TRUE           
                        SET SO-INVALID-COMPUTER-MAP   TO TRUE           
                                                                        
                        DISPLAY '2005 AFTER 2309: '                     
                        DISPLAY 'SW-IF-COMPUTER-MAP-INVALID '           
                                  SW-IF-COMPUTER-MAP-INVALID            
                        DISPLAY 'SO-TRY-GENERATE-MAP-AGAIN '            
                                  SO-TRY-GENERATE-MAP-AGAIN             
                 END-IF                                                 
      * PARAGRAPH WILL BE COUNTING HOW MANY ITERATIONS WE HAD           
      * AND IF THIS NUMBER WILL BE GRATER THAN 2500 THEN IT WILL BREATE 
      * THE LOOP                                                        
                 PERFORM 2330-CHECK-LOOP-ITERATIONS                     
                 DISPLAY 'IN LOOP IN 2005 POWODY: '                     
                 DISPLAY '2005 IF ALL PLACED: '                         
                         SW-IF-ALL-SHIPS-ARE-PLACED                     
                 DISPLAY '2005 IF COMPUTER MAP VALID : '                
                         SW-IF-COMPUTER-MAP-INVALID                     
                 DISPLAY '2005 IF COMPUTER GENERATED: '                 
                 SW-IF-COMPUTER-MAP-GENERATED                           
              END-PERFORM                                               
           END-PERFORM                                                  
                                                                        
                                                                        
      * THIS IF STATEMENT WILL BE TRUE ONLY WHEN PROGRAM TRYIED TO      
      * CREATE COMPUTER MAP FOR OVER 5 TIMES AND IT WAS NOT SUCCESSFULL 
      *                                                                 
                                                                        
           IF WS-NUMBER-OF-CYCLES > CT-MAXIMAL-NUMBER-OF-CYCLES AND     
              NOT SO-COMPUTER-MAP-WAS-GENERATED THEN                    
               DISPLAY 'COMPUTER MAX COULD NOT BE GENERATED '           
               MOVE 'INVALID COMPUTER MAP '         TO MSGO             
               PERFORM 2100-SEND-THE-MAP                                
               SET     SO-TERMINATION-WITHOUT-MESS  TO TRUE             
               SET     SO-INVALID-COMPUTER-MAP      TO TRUE             
           END-IF                                                       
           DISPLAY 'LOOP END  '                                         
                                                                        
      * TEST                                                            
           DISPLAY 'TUTAJ WYSWIETLAM WSZYSTKIE STATKI: '                
           DISPLAY WS-SCREEN-LINE(1)                                    
           DISPLAY WS-SCREEN-LINE(2)                                    
           DISPLAY WS-SCREEN-LINE(3)                                    
           DISPLAY WS-SCREEN-LINE(4)                                    
           DISPLAY WS-SCREEN-LINE(5)                                    
           DISPLAY WS-SCREEN-LINE(6)                                    
           DISPLAY WS-SCREEN-LINE(7)                                    
           DISPLAY WS-SCREEN-LINE(8)                                    
           DISPLAY WS-SCREEN-LINE(9)                                    
           DISPLAY WS-SCREEN-LINE(10)                                   
      * / TEST                                                          
           .                                                            
      ******************************************************************
      *                   2006-GET-RANDOM-POSITION                      
      * PARAGRAPH IS CALLED WHEN PROGRAM NEEDS TO GET A RANDOM          
      * VALUE BETWEEN 0 AND 99                                          
      *                                                                 
      *     
      ******************************************************************
       2006-GET-RANDOM-POSITION.                                        
           DISPLAY '2006-GET-RANDOM-POSITION IS CALLED '                
           COMPUTE WS-RANDOM-VALUE =                                    
                             FUNCTION RANDOM * 1000000                  
           DISPLAY '2006 WS-RANCOM-VALUE AFTER MULTIPLAYING: '          
                                  WS-RANDOM-VALUE                       
           COMPUTE WS-RANDOM-VALUE  = WS-RANDOM-VALUE / 100             
           DISPLAY '2006 WS-RANDOM-VALUE AFTER DIVISION: '              
                                  WS-RANDOM-VALUE                       
           DISPLAY '2006 RANDOM-VALUE '   WS-RANDOM-VALUE               
           .                                                            
      ******************************************************************
      *                   2007-GET-FIRST-SEED                           
      * PARAGRAPH IS USED IN ORDER TO START A RANDOM SEQUENCE           
      *                                                                 
      * HERE PROGRAM WILL ISSUE RANDOM FUNCTION WITH A SEED SPECIFIED   
      * THIS IS NEEDED ONLY FOR FIRST RANDOM FUNCTION                   
      *                                                                 
      * LATER WE WILL USER JUST FUNTION RANDOM (WITHOUT ANY DATA)       
      ******************************************************************
       2007-GET-FIRST-SEED.                                             
           DISPLAY '2007-GET-FIRST-SEED PERFORMED'                      
           PERFORM 2173-GET-THE-SEED                                    
                                                                        
           COMPUTE WS-RANDOM-VALUE = FUNCTION RANDOM(WS-FIRST-SEED)     
                                             * 1000000                  
           DISPLAY '2007 WS-RANODM-VALUE AFTER MULTIPLICAITON: '        
                   WS-RANDOM-VALUE                                      
           COMPUTE WS-RANDOM-VALUE = WS-RANDOM-VALUE / 100              
           DISPLAY '2007 WS-RANODM-VALUE AFTER DIVISION: '              
                   WS-RANDOM-VALUE                                      
                                                                        
           DISPLAY '2007 WS-FIRST-SEED  : '   WS-FIRST-SEED             
           DISPLAY '2007 WS-RANDOM-VALUE: '   WS-RANDOM-VALUE           
           .                                                            
          
      ******************************************************************
      *                   2008-GET-RANDOM-ORIENTATION                   
      * WE WILL GET VALUE FROM 0 TO 1                                   
      *                                                                 
      * IF THIS VALUE WILL BE LESS THAN 0.5 THEN WE WILL PLACE A SHIP   
      * VERTICALY                                                       
      * AND IN OTHER CASE HORIZONTALY                                   
      ******************************************************************
       2008-GET-RANDOM-ORIENTATION.                                     
           DISPLAY 'PARAGRAPH 2008-GET-RANDOM-ORIENTATION    IS CALLED' 
           COMPUTE WS-RANDOM-ORIENTATION =                              
                             FUNCTION RANDOM                            
           DISPLAY '2008 WS-RANDOM-ORIENTATION: ' WS-RANDOM-ORIENTATION 
           IF WS-RANDOM-ORIENTATION < 0.5 THEN                          
              SET SO-VERTICAL-ORIENTATION TO TRUE                       
              DISPLAY ' 2008 VERTICAL ORIENTATION '                     
           ELSE                                                         
              SET SO-HORIZONTAL-ORIENTATION TO TRUE                     
              DISPLAY ' 2008 HORIZONTAL ORIENTATION '                   
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2009-PREPARE-SHIP-DATA                        
      * PARAGRAPH WILL CHECK IF THE GIVEN SHAPE CAN BE PLACED IN        
      * A GIVEN POSITION AND GIVEN ORIENTATION                          
      *                                                                 
      * PARAGRAPH 2012-VALIDATE-THE-SHIP WILL ONLY VALIDATE             
      * I A SHAPE FITS COMPLETLY ON THE BOARD                           
      *                                                                 
      * WHILE PLACING A SHAPE WE NEED TO CHECK IF ANY OF THE SHIP'S     
      *   FIELD DOSEN'T HAVE  A NEIGHBOR HORIZONTALY OR VERTIACLY       
      *                                                                 
      *                                                                 
      ******************************************************************
       2009-PREPARE-SHIP-DATA.                                          
           DISPLAY '2009-PREPARE-SHIP-DATA IS CALLED '                  
                                                  
           PERFORM 2010-CALCULATE-POSITION                              
           PERFORM 2012-VALIDATE-THE-SHIP                               
           .                                                            
      ******************************************************************
      *                  2010-CALCULATE-POSITION                        
      *                                                                 
      * AT THE BEGINING OF THIS PARAGRAPH WE HAVE WS-RANDOM-VALUE       
      * THAT WILL STORE VALUE FROM 1 TO 100                             
      * PARAGRAPH WILL HAVE TO CALULCATE NUMBER OF ROW AND NUMBER OF    
      * COLUMN FROM THAT VALUE                                          
      *                                                                 
      * WHEN WS-RANDOM-VALUE IS LESS THAN 100 THEN PROGRAM LOGIC        
      * LOOK LIKE THIS                                                  
      *         XY   - LETS SAY IT IS OUR NUMBER IF IT IS LESS THAN 10  
      *                      THEN X = 0 AND Y IS SOME VALUE             
      *                                                                 
      * PARAGRAPH WILL TAKE X AND Y SEPARETLY AND WILL ADD 1 TO X       
      * AND 1 TO Y                                                      
      *                                                                 
      * SO FOR EXMAPLE IF WE GOT 25                                     
      *                                                                 
      * WE WILL GET X = 3   AND Y = 6                                   
      * WHERE X IS OUR ROW NUMBER                                       
      * AND Y IS OUR COLUMN NUMBER                                      
      *                                                                 
      * WE HAVE TO ADD 1 TO THOSE NUMBERS BECAUSE WHEN                  
      *     XY = 05                                                     
      *                                                                 
      * WE WOULD GET ROW NUMBER OF ZERO WHICH IS INVALID                
      *                                                                 
      * THIS PROBLEM IS OUT OF SCOPE WHEN WE WILL GET NUMBER            
      * LIKE 99 BECAUSE WE CAN HAVE VALUE OF 10 IN ROW AND COLUMN       
      *                                                                 
      * BUT WHEN OUR NUMBER IS 100 THEN WE WILL HAVE                    
      * TO MANUALLY PLACE THERE X = 10 AND Y = 10 BECAUSE               
      * OUR LOGIC CANT DEAL WITH IT                                     
      ******************************************************************
       2010-CALCULATE-POSITION.                                         
           DISPLAY ' 2010-CALCULATE-POSITION IS PERFORMED'              
           DISPLAY '2010  RANDOM-VALUE : ' WS-RANDOM-VALUE              
           IF WS-RANDOM-VALUE = 100 THEN                                
              DISPLAY '2010 WS-RANDOM-VALUE = 100 '                     
              MOVE 10 TO WS-COLUMN-POSITION                             
              MOVE 10 TO WS-ROW-POSITION                                
           ELSE                                                         
              DISPLAY 'WS-RANDOM-VALUE NOT = 100 '                      
              MOVE WS-RANDOM-VALUE         TO WS-RANDOM-VALUE-TEMP      
              MOVE WS-RANDOM-VALUE-TEMP    TO WS-RANDOM-VALUE-TEXT      
              COMPUTE WS-COLUMN-POSITION = FUNCTION NUMVAL(             
               WS-RANDOM-VALUE-SECOND-NUMBER  )                         
              COMPUTE WS-ROW-POSITION =    FUNCTION NUMVAL(             
                WS-RANDOM-VALUE-FISRT-NUMBER  )                         
               ADD 1 TO WS-COLUMN-POSITION                              
               ADD 1 TO WS-ROW-POSITION                                 
               DISPLAY '2010 WS-COLUMN-POSITION: ' WS-COLUMN-POSITION   
               DISPLAY '2010 WS-ROW-POSITION: '    WS-ROW-POSITION      
           END-IF                                                       
           DISPLAY '2010 CALUCLATED POSITIONS: '                        
           DISPLAY 'ROW ' WS-ROW-POSITION 'COLUMN ' WS-COLUMN-POSITION  
           .                                                            
      ******************************************************************
      *                   2011-INITALIZE-FLAGS                          
      * PROGRAM WILL INITIALIZE FLAGS THAT WILL BE NEEDED WHILE         
      * GENRATING COMPUTER'S SHIPS                                      
      ******************************************************************
       2011-INITALIZE-FLAGS.                                            
           SET SO-THIS-TYPE-NOT-PLACED(2)  TO TRUE                      
           SET SO-THIS-TYPE-NOT-PLACED(3)  TO TRUE                      
           SET SO-THIS-TYPE-NOT-PLACED(4)  TO TRUE                      
           SET SO-THIS-TYPE-NOT-PLACED(5)  TO TRUE                      
           SET SO-NOT-ALL-SHIPS-ARE-PLACED TO TRUE                      
           SET SO-COMPUTER-MAP-IS-VALID    TO TRUE                      
           .                                                            
      ******************************************************************
      *                    2012-VALIDATE-THE-SHIP                       
      * PARAGRAPH WILL CHECK IF THERE IS ANY TYPE OF THE SHIP           
      * THAT CAN BE PLACED ON THE BOARD                                 
      *   IF THERE IS ANY SHIP THAT MEETS CRITERIA PARAGRAPH WILL       
      * CHECK IF THIS IS POSSIBLE TO PLACE A SHIP IN A GIVEN POSITION   
      *                                                                 
      *                                                                 
      *   IF INDEX OF THE ARRAY IS EQUAL TO 5 THEN IT MEANS THAT        
      * THIS IS THE 5 FIELDS LONG SHIP                                  
      *                                                                 
      *   IF INDEX OF THE ARRAY IS EQUAL TO 4 THEN IT MEANS THAT        
      * THIS IS THE 4 FIELDS LONG SHIP                                  
      *                                                                 
      *   IF INDEX OF THE ARRAY IS EQUAL TO 3 THEN IT MEANS THAT        
      * THIS IS THE 3 FIELDS LONG SHIP                                  
      *                                                                 
      *   IF INDEX OF THE ARRAY IS EQUAL TO 2 THEN IT MEANS THAT        
      * THIS IS THE 2 FIELDS LONG SHIP                                  
      *                                                                 
      ******************************************************************
       2012-VALIDATE-THE-SHIP.                                          
           DISPLAY '2012-VALIDATE-THE-SHIP PERFORMED'                   
                                                                        
           IF WS-THIS-TYPE-SHIP-COUNTER(5)  >=                          
              WS-MAXIMAL-AMOUNT-OF-SHIPS(5)   AND                       
              WS-THIS-TYPE-SHIP-COUNTER(4)  >=                          
              WS-MAXIMAL-AMOUNT-OF-SHIPS(4)   AND                       
              WS-THIS-TYPE-SHIP-COUNTER(3)  >=                          
              WS-MAXIMAL-AMOUNT-OF-SHIPS(3)   AND                       
              WS-THIS-TYPE-SHIP-COUNTER(2)  >=                          
              WS-MAXIMAL-AMOUNT-OF-SHIPS(2) THEN                        
               SET SO-ALL-SHIPS-ARE-PLACED       TO TRUE                
               SET SO-COMPUTER-MAP-WAS-GENERATED TO TRUE                
               DISPLAY 'WSZYSTKIE STATKI DODANE '                       
           ELSE                                                         
            SET SO-NOT-PLACED-NOT-FOUND TO TRUE                         
      * THIS LOOP GOES FROM THE SMALLEST POSSIBLE SHIP TO THE           
      * BIGGEST POSSIBLE SHIP                                           
      * SMALLEST ONE IS 2 FIELD LONG AND LONGEST ONE IS                 
      * 5 FIELDS LONG                                                   
            PERFORM VARYING WS-TYPE-OF-SHIP FROM 2                      
            BY 1 UNTIL WS-TYPE-OF-SHIP > 5 OR SO-NOT-PLACED-FOUND       
                                                                        
             IF WS-THIS-TYPE-SHIP-COUNTER(WS-TYPE-OF-SHIP)  <           
                WS-MAXIMAL-AMOUNT-OF-SHIPS(WS-TYPE-OF-SHIP) THEN        
                                                                        
                                                                        
               MOVE WS-TYPE-OF-SHIP TO WS-FIELD-NUMBER                  
      * PARAGRAPH WILL CHECK IF SHIP CAN BE PLACED IN A GIVEN           
      * POSITION WITH A GIVEN ORIENTATION                               
               PERFORM 2013-IF-SHIP-CAN-BE-IN-POS                       
               SET SO-NOT-PLACED-FOUND TO TRUE                          
                                                                        
             END-IF                                                     
            END-PERFORM                                                 
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2013-IF-SHIP-CAN-BE-IN-POS                    
      * PARAGRAPH WILL CHECK IF THIS IS POSSIBLE TO PLACE               
      * A SHIP IN A GIVEN FIELD ON THE BOARD WITH THE GIVEN ORIENTATION 
      * AND WITH GIVEN LENGTH OF A SHIP ( FOR EXAMPLE SHIP CAN          
      * HAVE A LENGTH OF 5 ( WHEN IT IS MADE OUT OF 5 FIELDS ))         
      ******************************************************************
       2013-IF-SHIP-CAN-BE-IN-POS.                                      
           DISPLAY '2013-IF-SHIP-CAN-BE-IN-POS PERFORMED'               
           DISPLAY '2013 X: ' WS-COLUMN-POSITION                        
           DISPLAY '2013 Y: ' WS-ROW-POSITION                           
           DISPLAY '2013 WS-FILEDNUMBER: ' WS-FIELD-NUMBER              
           EVALUATE TRUE                                                
      * IF SHIP ORIENTATION IS VERTICAL THEN PARAGRAPH                  
      * WILL CHECK IF POSITION WHEN THIS SHIPS STARTS (                 
      * HIS ROW POSITION TO BE PRECISE)                                 
      * PLUS HIS LENGTH IS LESS OR MORE THAN MAXIMAL HEIGHT OF THE      
      * SCREEN                                                          
      * IF THIS VALUE IS LESS OR EQUAL THEN THIS SHIP CAN BE PLACED     
      * IN THIS POSITION                                                
      *                                                                 
      * WS-FIELD-NUMBER VARIABLE STORES INFO ABOUT LENGTH OF THE        
      * SHIP                                                            
           WHEN  SO-VERTICAL-ORIENTATION                                
                DISPLAY 'VERTICAL  WS-FIELD-NUMBER: '  WS-FIELD-NUMBER  
                                                                        
                COMPUTE WS-TEMP-NUMERIC = WS-ROW-POSITION +             
                              WS-FIELD-NUMBER                           
                DISPLAY 'WS-TEMP-NUMERIC: ' WS-TEMP-NUMERIC             
                IF  WS-TEMP-NUMERIC <= CT-MAXIMAL-HEIGHT-OF-BORAD THEN  
                   DISPLAY '2013 VERTICAL SHIP CAN BE PLACED '          
                   SET SO-SHIP-CAN-BE-PLACED TO TRUE                    
                ELSE                                                    
                   DISPLAY '2013 VERTICAL SHIP CANT BE PLACED '         
                   SET SO-SHIP-CANT-BE-PLACED TO TRUE                   
                END-IF                                                  
      * IF SHIP ORIENTATION IS HORIZONTAL THEN PARAGRAPH                
      * WILL CHECK IF POSITION WHEN THIS SHIPS STARTS (                 
      * HIS COLUMN  POSITION TO BE PRECISE)                             
      * PLUS HIS LENGTH IS LESS OR MORE THAN MAXIMAL WIDTH OF THE       
      * SCREEN                                                          
      * IF THIS VALUE IS LESS OR EQUAL THEN THIS SHIP CAN BE PLACED     
      * IN THIS POSITION                                                
      *                                                                 
      * WS-FIELD-NUMBER VARIABLE STORES INFO ABOUT LENGTH OF THE        
      * SHIP                                                            
           WHEN  SO-HORIZONTAL-ORIENTATION                              
                DISPLAY 'HORIZONTAL  WS-FIELD-NUMBER: ' WS-FIELD-NUMBER 
                IF WS-COLUMN-POSITION + WS-FIELD-NUMBER <=              
                     CT-MAXIMAL-WIDTH-OF-BOARD THEN                     
                   SET SO-SHIP-CAN-BE-PLACED TO TRUE                    
                ELSE                                                    
                   SET SO-SHIP-CANT-BE-PLACED TO TRUE                   
                END-IF                                                  
           WHEN OTHER                                                   
              MOVE 'ERROR IN 2013 ' TO MSGO                             
              PERFORM 2100-SEND-THE-MAP                                 
           END-EVALUATE                                                 
      * IF SHIP CAN BE PLACED IN A GIVEN POSITION WE WILL ALSO          
      * CHECK IF THIS FIELD IS EMPTY OR NOT                             
      * IF THERE IS 'S' THEN IT MEANS THAT POSITION IS TAKEN            
           IF SO-SHIP-CAN-BE-PLACED THEN                                
               DISPLAY '2013 AFTER EVALUATE SO-SHIP-CAN-BE-PLACED'      
               MOVE  WS-SCREEN-LINE(WS-ROW-POSITION)(                   
                         WS-COLUMN-POSITION:1) TO SW-WHAT-TYPE-OF-FIELD 
               DISPLAY '2013 SW-WHAT-TYPE-OF-FIELD: '                   
                        SW-WHAT-TYPE-OF-FIELD                           
               IF SO-SHIP-FIELD                                         
                 THEN                                                   
                   DISPLAY ' 2013 THIS POSITION IS TAKEN '              
                   SET SO-SHIP-CANT-BE-PLACED TO TRUE                   
               END-IF                                                   
                                                                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2014-PLACE-THE-SHIP                           
      * THIS PARAGRAPH CAN BE USED IN ORDER TO PLACE ANY SHIP           
      * ANYWHERE ON THE MAP (VALIDATION IF A SHIP CAN BE PLACED         
      * IN A GIVEN POSITION WITH A GIVEN ORIENTATION SHOULD BE MADE     
      * BEFORE PERFORMING THIS PARAGRAPH)                               
      ******************************************************************
       2014-PLACE-THE-SHIP.                                             
           DISPLAY '2014 PLACE THE SHIP PERFORMED'                      
           SET SO-SHIP-WAS-NOT-PLACED TO TRUE                           
           SET SO-SHIP-POSITION-VALID TO TRUE                           
           DISPLAY '2014 SW-IF-SHIP-WAS-PLACED '                        
                   SW-IF-SHIP-WAS-PLACED                                
           DISPLAY '2014 SW-IF-SHIP-POSTION-VALID '                     
                   SW-IF-SHIP-POSTION-VALID                             
      * INITIALIZATION OF ARRAY                                         
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
               MOVE SPACE TO WS-SCREEN-LINE2(WS-ITER5)                  
               DISPLAY 'WS-SCREEN-LINE2(WS-ITER5) : '                   
                            WS-SCREEN-LINE2(WS-ITER5)                   
           END-PERFORM                                                  
                                                                        
                                                                        
           DISPLAY 'ZACZYNAM 2014 WS-FIELD NUMBER:  ' WS-FIELD-NUMBER   
                                                                        
           EVALUATE TRUE                                                
           WHEN SO-VERTICAL-ORIENTATION                                 
               DISPLAY '2014 SO VERTICAL ORIENTATION '                  
               PERFORM 2235-PLACE-VERTICAL-SHIP                         
           WHEN SO-HORIZONTAL-ORIENTATION                               
               DISPLAY '2014 SO HORIZONTAL  ORIENTATION '               
               PERFORM 2236-PLACE-HORIZONTAL-SHIP                       
           WHEN OTHER                                                   
               DIPSLAY '2014 AT END: '                                  
               MOVE 'OTHER ERROR IN 2014 ' TO MSGO                      
               DISPLAY 'MSGO: ' MSGO                                    
               PERFORM 2100-SEND-THE-MAP                                
               SET SO-SHIP-WAS-NOT-PLACED TO TRUE                       
               DISPLAY 'OTHER: ' SW-IF-SHIP-WAS-PLACED                  
           END-EVALUATE                                                 
           DISPLAY '2014 WS-SHIP-COUNTER ' WS-SHIP-COUNTER              
                                                                        
           IF SO-SHIP-WAS-PLACED THEN                                   
              DISPLAY '2014 AT END SO-SHIP-WAS-PLACED '                 
              DISPLAY '2014 IF SHIP WAS PLACED'                         
              PERFORM 2237-COUNT-TYPES-OF-SHIPS                         
                                                                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2015-INITIALIZE-SCREEN-ARRAY                   
      ******************************************************************
       2015-INITIALIZE-SCREEN-ARRAY.                                    
           DISPLAY '2015-INITIALIZE-SCREEN-ARRAY PERFORMED'             
           PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 >   10   
                                                                        
              MOVE SPACE      TO WS-SCREEN-LINE(WS-ITER2)               
              MOVE SPACE      TO WS-SCREEN-LINE2(WS-ITER2)              
              DISPLAY '2015 INITIALIZE BOTH ARRAYS : '                  
              DISPLAY 'LINE 1: '  WS-SCREEN-LINE(WS-ITER2)              
              DISPLAY 'LINE 2: '  WS-SCREEN-LINE2(WS-ITER2)             
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2016-VALIDATE-NEIGHBORS                        
      * THIS PARAGRAPH WILL CHECK IF SHIP CAN BE PLACED IN A GIVEN      
      * FIELD ( IT WILL CHECK IF THERE ARE INVALID NEIGHBOURS NEXT      
      * TO THIS FIELD)                                                  
      *                                                                 
      * FIELD CANT HAVE NEIGBHOURS VERTICALLY AND HORIZONTALLY BUT IT   
      * CAN HAVE NEIGHBOURS DIAGONALLY                                  
      *                                                                 
      *                                                                 
      *   LOOP IN THIS PARAGRAPH WILL ITERATE ON                        
      *    WS-COLUMN-ITERATOR AND WS-ROW-ITERATOR IN ORDER TO CALCUALTE 
      *      WS-TEMP-COLUMN AND WS-TEMP-ROW THAT WILL STORE POSTIION    
      *   OF THE NEIGHBOUR                                              
      *                                                                 
      * THEN PARAGRAPH 2017-CHECK-CALCULATED-POSITION   WILL BE PERFOMED
      *  TO CHECK IF NEIGHBOUR IS VALID OR NOT                          
      ******************************************************************
       2016-VALIDATE-NEIGHBORS.                                         
           MOVE WS-COLUMN-VALIDATION          TO WS-TEMP-COLUMN         
           MOVE WS-ROW-VALIDATION             TO WS-TEMP-ROW            
           SET  SO-SHIP-POSITION-VALID        TO TRUE                   
           DISPLAY '2016  WS-COLUMN-VALIDATION  '                       
                          WS-COLUMN-VALIDATION                          
           DISPLAY '2016  WS-ROW-VALIDATION     '                       
                          WS-ROW-VALIDATION                             
           DISPLAY '2016  WS-TEMP-COLUMN        '                       
                          WS-TEMP-COLUMN                                
           DISPLAY '2016  WS-TEMP-ROW           '                       
                          WS-TEMP-ROW                                   
           DISPLAY '2016 SW-IF-SHIP-POSTION-VALID '                     
                 SW-IF-SHIP-POSTION-VALID                               
                                                                        
           PERFORM VARYING WS-COLUMN-ITERATOR FROM -1 BY 1 UNTIL        
                   WS-COLUMN-ITERATOR > 1 OR  SO-SHIP-POSITION-INVALID  
                                                                        
               PERFORM VARYING WS-ROW-ITERATOR FROM -1 BY 1 UNTIL       
                   WS-ROW-ITERATOR > 1 OR  SO-SHIP-POSITION-INVALID     
                                                                        
      * WE WILL CHECK THE HORIZONTAL AND VERTICAL NEIGHBOURS            
      * AND IN ORDER TO DO THAT WE  HAVE TO GET VALID COLUMN AND ROW    
      * ITERATORS, WE WILL USE THOSE ITERATORS TO CALCULATE POSTIIONS   
      * OF THE NEIGBOURS                                                
      *                                                                 
      * TO CHECK HORIZONTAL NEIGHBOURS WE WILL HAVE TO CHECK            
      * POSTISIONS WHERE COLUMN WILL BE GRATER BY 1 THAN OUR FIELD      
      * AND WHERE COLUMN WILL BE LESS BY 1 THAN OUR FIELD               
      * IN THE SAME TIME ROW NEEDS TO BE THE SAME                       
      *                                                                 
      * SAME LOGIC WILL BE TRUE IN ORDER TO GET VERTICAL NEIGBORS       
      * WE WILL AHVE TO CHECK FIELD THAT HAS ROW NUMBER GRATER THAN     
      * OUR FIELD + 1 AND -1, IN THAT TIME COLUMN NUMBER                
      * CANT BE CHANGED                                                 
      *                                                                 
      * EVALUATE STAEMENT BELOW WILL CHECK THE NEIGBOURS ONLY WHEN      
      * WS-ROW-ITERATOR = 1 OR -1 AND WS-COLUMN-ITERATOR = 0            
      * OR                                                              
      * WS-COLUMN-ITERATOR = 1 OR -1 AND WS-ROW-ITERATOR = 0            
      *                                                                 
      * THANKS TO THAT IF STATEMENT WE WILL ONLY CHECK VALID NEIGHBOURS 
                  IN                                                    
                  EVALUATE TRUE                                         
                    WHEN ( WS-COLUMN-ITERATOR = 1 OR -1 ) AND           
                          WS-ROW-ITERATOR = 0                           
                    WHEN ( WS-ROW-ITERATOR = 1 OR -1 ) AND              
                          WS-COLUMN-ITERATOR = 0                        
                        MOVE WS-ROW-ITERATOR   TO WS-ROW-ITERATOR-FORMAT
                        MOVE WS-COLUMN-ITERATOR TO                      
                                                  WS-COL-ITERATOR-FORMAT
                        DISPLAY '2016 WS-ROW-ITERATOR: '                
                           WS-ROW-ITERATOR-FORMAT                       
                        DISPLAY '2016 WS-COLUMN-ITERATOR: '             
                           WS-COL-ITERATOR-FORMAT                       
                        DISPLAY '2016 WS-ROW-ITERATOR    '              
                                      WS-ROW-ITERATOR                   
                        DISPLAY '2016 WS-COLUMN-ITERATOR '              
                                      WS-COLUMN-ITERATOR                
                                                                        
                        ADD WS-COLUMN-ITERATOR TO WS-TEMP-COLUMN        
                        ADD WS-ROW-ITERATOR    TO WS-TEMP-ROW           
                        DISPLAY '2016 WS-TEMP-COLUMN:  ' WS-TEMP-COLUMN 
                        DISPLAY '2016 WS-TEMP-ROW: '     WS-TEMP-ROW    
                                                                        
                        PERFORM 2017-CHECK-CALCULATED-POSITION          
                    WHEN OTHER                                          
      *                 CONTINUE                                        
                        DISPLAY '2016 "OTHER" IN EVALUATE STATEMENT NO '
                                'ACTION '                               
                  END-EVALUATE                                          
      * AT THE END OF LOOP ITERATION PARAGRAPH WILL RESET WS-TEMP-COLUM 
      * ADN WS-TEMP-ROW TO OUR FIELD POSITION                           
                  MOVE WS-COLUMN-VALIDATION          TO WS-TEMP-COLUMN  
                  MOVE WS-ROW-VALIDATION             TO WS-TEMP-ROW     
                  DISPLAY '2016 END OF THE LOOP : '                     
                  DISPLAY '2016 WS-COLUMN-VALIDATION '                  
                                WS-COLUMN-VALIDATION                    
                  DISPLAY '2016 WS-ROW-VALIDATION    '                  
                                WS-ROW-VALIDATION                       
                  DISPLAY '2016 WS-TEMP-COLUMN       '                  
                                WS-TEMP-COLUMN                          
                  DISPLAY '2016 WS-TEMP-ROW          '                  
                                WS-TEMP-ROW                             
               END-PERFORM                                              
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                 2017-CHECK-CALCULATED-POSITION                  
      * PARAGRAPH IS USED TO CHECK IF NEIGHBOURS OF THE FIELD IS        
      * VALID OR NOT ( IF NEIGHBOUR OF OUR FIELD IS A 'X' OR 'S' THEN   
      * IT MEANS THAT THIS FIELD IS INVALID ( WE CANNOT PLACE A SHIP    
      * THERE )                                                         
      *                                                                 
      * PARAGRAPH WILL EXECUTE ITS LOGIC ONLY WHEN CALCUALTED POSITION  
      * IS VALID SHIP POSITION ( IT IS VALID SCREEN POSITION )          
      ******************************************************************
       2017-CHECK-CALCULATED-POSITION.                                  
           DISPLAY '2017-CHECK-CALCULATED-POSITION  PERFORMED'          
      * IF STATEMENT BELOW WILL CHECK IF CALCUALTED POSITION            
      * IS VALID POSTION ( IF IS GRATER THAN 1 AND LESS THATN MAXIMAL   
      * HEIGHT OR MAXIMAL WIDTH OF THE SCREEN)                          
                                                                        
           IF WS-TEMP-ROW >= 1 AND WS-TEMP-ROW <=                       
                                         CT-MAXIMAL-HEIGHT-OF-BORAD  AND
              WS-TEMP-COLUMN >=1 AND WS-TEMP-COLUMN <=                  
                                         CT-MAXIMAL-WIDTH-OF-BOARD  THEN
             DISPLAY '2017 IN THE IF STATEMENT '                        
             DISPLAY '2017 VALEUS : '                                   
             DISPLAY '2017 ROW NUMBER:  '  WS-TEMP-ROW                  
             DISPLAY '2017 COLUMN NUMBER: ' WS-TEMP-COLUMN              
             DISPLAY '2017 WS-COLUMN-VALIDATION ' WS-COLUMN-VALIDATION  
             DISPLAY '2017 WS-ROW-VALIDATION    ' WS-ROW-VALIDATION     
                                                                        
             MOVE WS-SCREEN-LINE(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) TO      
                                   SW-WHAT-TYPE-OF-FIELD                
             DISPLAY '2017 SW-WHAT-TYPE-OF-FIELD '                      
             EVALUATE TRUE                                              
               WHEN SO-HIT-FIELD                                        
               WHEN SO-SHIP-FIELD                                       
                 DISPLAY '2017 POSITION INVALID '                       
                 SET SO-SHIP-POSITION-INVALID TO TRUE                   
               WHEN OTHER                                               
                 DISPLAY '2017 POSITION VALID '                         
                 SET SO-SHIP-POSITION-VALID   TO TRUE                   
             END-EVALUATE                                               
           ELSE                                                         
             DISPLAY '2017 PROGRAM DIDNT PERFORM THE IF STATEMENT '     
             DISPLAY '2017 NO ACTION '                                  
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2026-CHECK-LEFT-NEIGHBOR                     
      ******************************************************************
       2026-CHECK-LEFT-NEIGHBOR.                                        
           DISPLAY ' 2026  CHECH LEFT- NEIGHBOR PERFORMED'              
           IF SO-SHIP-POSITION-VALID THEN                               
            DISPLAY '2026 SO-SHIP-POSITION-VALID  '                     
            COMPUTE WS-COLUMN-VALIDATION-TEMP = WS-COLUMN-VALIDATION - 1
                                                                        
            DISPLAY '2026  WS-COLUMN-VALIDATION-TEMP AFTER COMPUTE'     
                          WS-COLUMN-VALIDATION-TEMP                     
                                                                        
            IF WS-SCREEN-LINE(WS-ROW-VALIDATION)(                       
               WS-COLUMN-VALIDATION-TEMP:1) = 'S' OR 'X'   THEN         
                DISPLAY '2062 IN IF STATMENT LEFT  NEIGHBOR DROP '      
                SET SO-SHIP-POSITION-INVALID   TO TRUE                  
                DISPLAY 'SW-IF-SHIP-POSTION-VALID  '                    
                        SW-IF-SHIP-POSTION-VALID                        
            ELSE                                                        
                DISPLAY '2062 NOT IN IF STATMENT  '                     
                SET SO-SHIP-POSITION-VALID TO TRUE                      
                DISPLAY 'SW-IF-SHIP-POSTION-VALID  '                    
                        SW-IF-SHIP-POSTION-VALID                        
            END-IF                                                      
                                                                        
           ELSE                                                         
      *  WE WILL NOT CHECK ANYTHING IF WE ALREADY KNOW THAT THIS        
      *  POSITION IS INVALID                                            
              DISPLAY '2026 SO-SHIP-POSITION-INVALID NO ACTION '        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2027-CHECK-RIGHT-NEIGHBOR                    
      ******************************************************************
       2027-CHECK-RIGHT-NEIGHBOR.                                       
           DISPLAY ' 2027  CHECH RIGHT NEIGHBOR PERFORMED'              
           IF SO-SHIP-POSITION-VALID THEN                               
              DISPLAY '2027: SO-SHIP-POSITION-VALID '                   
             COMPUTE WS-COLUMN-VALIDATION-TEMP =                        
            WS-COLUMN-VALIDATION + 1                                    
              DISPLAY ' WS-COLUMN-VALIDATION-TEMP '                     
                      WS-COLUMN-VALIDATION-TEMP                         
                                                                        
            IF WS-SCREEN-LINE(WS-ROW-VALIDATION)(                       
            WS-COLUMN-VALIDATION-TEMP:1) =                              
               'S' OR 'X' THEN                                          
                DISPLAY 'IN IF STATMENT: RIGHT NEIGHBOR DROP '          
                SET SO-SHIP-POSITION-INVALID   TO TRUE                  
                DISPLAY 'SW-IF-SHIP-POSTION-VALID  '                    
                        SW-IF-SHIP-POSTION-VALID                        
            ELSE                                                        
                DISPLAY 'NOT IN IF STATMENT:  '                         
                SET SO-SHIP-POSITION-VALID TO TRUE                      
                DISPLAY 'SW-IF-SHIP-POSTION-VALID  '                    
                        SW-IF-SHIP-POSTION-VALID                        
            END-IF                                                      
           ELSE                                                         
      * WE WILL NOT CHECK ANYTHING IF WE ALREADY KNOW THAT THIS         
      * POSITION IS INVALID                                             
                                                                        
              DISPLAY '2027: SO-SHIP-POSITION-INVALID NO ACTION '       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2028-CHECK-TOP-NEIGHBOR                      
      ******************************************************************
       2028-CHECK-TOP-NEIGHBOR.                                         
           DISPLAY ' 2028  CHECH TOP   NEIGHBOR PERFORMED'              
           IF SO-SHIP-POSITION-VALID THEN                               
           DISPLAY ' 2028  SO-SHIP-POSITION-VALID '                     
             COMPUTE WS-ROW-VALIDATION-TEMP = WS-ROW-VALIDATION - 1     
            IF WS-SCREEN-LINE(WS-ROW-VALIDATION-TEMP)(                  
            WS-COLUMN-VALIDATION:1) =                                   
               'S' OR 'X' THEN                                          
                DISPLAY '2028 IN IF STATMENT TOP NEIGHBOR DROP '        
                SET SO-SHIP-POSITION-INVALID   TO TRUE                  
                DISPLAY 'SW-IF-SHIP-POSTION-VALID  '                    
                        SW-IF-SHIP-POSTION-VALID                        
            ELSE                                                        
                DISPLAY '2028 NOT IN IF STATMENT '                      
                SET SO-SHIP-POSITION-VALID TO TRUE                      
                DISPLAY 'SW-IF-SHIP-POSTION-VALID  '                    
                        SW-IF-SHIP-POSTION-VALID                        
            END-IF   
           ELSE                                                         
      * WE WILL NOT CHECK ANYTHING IF WE ALREADY KNOW THAT THIS         
      * POSITION IS INVALID                                             
             DISPLAY ' 2028  SO-SHIP-POSITION-INVALID NO ACTION '       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2029-CHECK-BOTTOM-NEIGHBOR                   
      ******************************************************************
       2029-CHECK-BOTTOM-NEIGHBOR.                                      
           DISPLAY ' 2029  CHECH TOP   NEIGHBOR PERFORMED'              
           IF SO-SHIP-POSITION-VALID THEN                               
           DISPLAY ' 2029  SO-SHIP-POSITION-VALID'                      
             COMPUTE WS-ROW-VALIDATION-TEMP = WS-ROW-VALIDATION + 1     
             DISPLAY 'WS-ROW-VALIDATION-TEMP: '                         
                        WS-ROW-VALIDATION-TEMP                          
            IF WS-SCREEN-LINE(WS-ROW-VALIDATION-TEMP)(                  
            WS-COLUMN-VALIDATION:1) =                                   
               'S' OR 'X' THEN                                          
                DISPLAY '2029 IN IF STATEMENT BOTTOM NEIGHBOR DROP '    
                SET SO-SHIP-POSITION-INVALID   TO TRUE                  
                DISPLAY 'SW-IF-SHIP-POSTION-VALID  '                    
                        SW-IF-SHIP-POSTION-VALID                        
            ELSE                                                        
                DISPLAY '2029 NOT IN IF STATEMENT  '                    
                SET SO-SHIP-POSITION-VALID TO TRUE                      
                DISPLAY 'SW-IF-SHIP-POSTION-VALID  '                    
                        SW-IF-SHIP-POSTION-VALID                        
            END-IF                                                      
           ELSE                                                         
      * WE WILL NOT CHECK ANYTHING IF WE ALREADY KNOW THAT THIS         
      * POSITION IS INVALID                                             
                                                                        
              DISPLAY ' 2029  SO-SHIP-POSITION-INVALID NO ACTION'       
           END-IF                                                       
           .    
      ******************************************************************
      *                     2030-SAVE-THE-BOARD                         
      * PARAGRAPH IS USED TO MOVE ONLY THE LETTER 'S' FROM ONE ARRAY    
      * INTO THE SECOND ARRAY ( ONLY LETTER 'S' CAN BE TRANSFERED       
      * ANY OTHER LETTER WILL STAY LIKE IT IS                           
      ******************************************************************
       2030-SAVE-THE-BOARD.                                             
           DISPLAY '2030-SAVE-THE-BOARD  IS PERFORMED'                  
                                                                        
           PERFORM VARYING WS-ITER-Y FROM 1 BY 1 UNTIL WS-ITER-Y > 10   
               PERFORM VARYING WS-ITER-X FROM 1 BY 1 UNTIL              
                               WS-ITER-X  > 10                          
                  IF WS-SCREEN-LINE2(WS-ITER-Y)(WS-ITER-X:1)  = 'S' THEN
                   DISPLAY 'IF STATEMENT IS CORRECT '                   
                   MOVE 'S' TO WS-SCREEN-LINE(WS-ITER-Y)(WS-ITER-X:1)   
                   DISPLAY 'WS-SCREEN-LINE(WS-ITER-Y)(WS-ITER-X:1) '    
                            WS-SCREEN-LINE(WS-ITER-Y)(WS-ITER-X:1)      
                  ELSE                                                  
                   DISPLAY '2030 NOT IN IF STATEMENT NO ACTION '        
                  END-IF                                                
               END-PERFORM                                              
           END-PERFORM                                                  
                                                                        
           PERFORM 2031-INITALIZE-THE-BOARD                             
           .                                                            
      ******************************************************************
      *                   2031-INITALIZE-THE-BOARD                      
      * PARAGRAPH IS CALLED IN ORDER TO INITIALZIE THE TEMP ARRAY       
      * THAT STORES INFO ABOUT COMPUTER'S BOARD                         
      *                                                                 
      * PROGRAM WILL GENERATE ONE SHIP ON THIS ARRAY AND WILL           
      * PLACE THIS SHIP INTO THE FINAL ARRAY                            
      * AFTER THAT TEMPORARY  ARRAY NEEDS TO BE RESET                   
      ******************************************************************
       2031-INITALIZE-THE-BOARD.                                        
           DISPLAY '2031-INITIALIZE-THE-BOARD PERFORMED'                
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3 > 10     
             MOVE SPACE      TO WS-SCREEN-LINE2(WS-ITER3)               
             DISPLAY 'WS-SCREEN-LINE2( ' WS-ITER3 ' ) '                 
                                 WS-SCREEN-LINE2(WS-ITER3)              
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2032-PROCESS-SINGLE-INPUT                    
      * PARAGRAPH WILL BE CALLED WHEN IT IS A SINGLEPLAYER MODE         
      *                                                                 
      * PARAGRAPH WILL :                                                
      * 1. IF USER DIDNT PLACE SHIPS IT WILL VALIDATE THEM              
      * 2. IF USER PLACED SHIPS IT WILL VALIDATE USER SHOT ( OR         
      * COMPUTER SHOT IF IT IS COMPUTER TURN)                           
      *                                                                 
      ******************************************************************
       2032-PROCESS-SINGLE-INPUT.                                       
           DISPLAY '2032-VALIDATE-USER-INPUT PERFORMED'                 
                                                                        
           IF SO-USER-DIDNT-PLACED-SHIPS THEN                           
              DISPLAY '2032 SO-USER-DIDNT-PLACED-SHIPS '                
              PERFORM 2320-CHECK-USERS-SHIPS                            
           ELSE                                                         
               DISPLAY 'SO USER ALREADY PLACED SHIPS '                  
      * WE WILL GET HERE IF THE GAME STARTED                            
               DISPLAY ' 2032 THIS IS THE GAME '                        
               PERFORM 2110-DATA-COMMAREA-TO-SCREEN                     
               PERFORM 2118-ENEMY-BOARD-TO-COMMON                       
      * DEPENDING ON THE FACT WHO HAS THE TURN PARAGRAPHS WILL BE       
      * PERFORMED                                                       
               PERFORM 2321-START-THE-SHOT-PROCEDURE                    
                                                                        
               DISPLAY 'ALMOST END OF 2032'                             
               PERFORM 2115-MOVE-COMMAREA-TO-SCREEN                     
      * PARAGRAPH WILL CHECK IF GAME ENDED                              
      * AND IF SO IT WILL PREPARE A USER MESSAGE                        
                                                                        
               PERFORM   2161-CHECK-IF-ENDGAME                          
               PERFORM   2105-PROTECT-USER-FIELDS                       
                                                                        
                                                                        
           END-IF                                                       
      * IF THIS IS A COMPUTER TURN THIS PIECE OF CODE WILL              
      * CREATE A LOOP ( COMPUTER WILL SHOOT AS MANY TIMES AS LONG       
      * HE HITS SOMETHING ) (IF HE MISSES THEN TURN GOES TO THE USER)   
           IF SO-TURN-CHANGED-TO-COMPUTER THEN                          
      * THANKS TO THIS PARAGRAPH COMPUTER WILL BE ABLE TO               
      * TO SHOT AS MANY TIMES AS LONG HE IS HITTING THE USER'S SHIPS    
      * (THAT LOGIC IS CREATED BY CALLING 2321-START-THE-SHOT-PROCEDURE 
      * PARAGRAPH )                                                     
             DISPLAY '2032 SO-TURN-CHANGED-TO-COMPUTER '                
             PERFORM 2322-COMPUTER-SHOTS-IN-LOOP                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2033-VALIDATE-USER-SHIPS                       
      *                                                                 
      ******************************************************************
       2033-VALIDATE-USER-SHIPS.                                        
      * PROGRAM WILL MOVE SCREEN VARIABLES TO PROGRAM VARIABLES         
           DISPLAY '2033-VALIDATE-USER-SHIPS PERFORMED '                
                                                                        
           PERFORM 2034-MOVE-USER-BOARD-TO-PROG                         
                                                                        
           PERFORM 2035-CHECK-THE-SHIPS                                 
                                                                        
           IF SO-VALID-USER-SHIPS THEN                                  
              DISPLAY '2033 SO-VALID-USER-SHIPS '                       
              PERFORM 2109-GET-VALUE-WHO-STARTS                         
              IF SO-COMPUTER-STARTS THEN                                
                DISPLAY '2033 SO-COMPUTER-STARTS   '                    
                MOVE 'PLEASE PRESS ENTER - FIRST TURN: COMPUTER ' TO    
                  MSGO                                                  
                SET SO-IT-IS-COMPUTERS-TURN  TO TRUE                    
                DISPLAY '2033 MSGO: ' MSGO                              
                DISPLAY '2033 WHOSE TURN: '    SW-WHOSE-TURN            
              ELSE                                                      
                DISPLAY '2033 SO-USER HAS THE TURN '                    
                MOVE 'PLEASE PRESS ENTER - FIRST TURN: YOU      ' TO    
                  MSGO                                                  
                SET SO-IT-IS-USERS-TURN      TO TRUE                    
                DISPLAY '2033 MSGO: ' MSGO                              
                DISPLAY '2033 WHOSE TURN: '    SW-WHOSE-TURN            
              END-IF                                                    
              DISPLAY ' 2033 END OF THE IF STATEMENT '                  
              SET SO-USER-PLACED-SHIPS       TO TRUE                    
              PERFORM 2105-PROTECT-USER-FIELDS                          
              PERFORM 2108-SAVE-USER-BOARD                              
              DISPLAY '2033: SW-IF-USER-PLACED-SHIPS '                  
                             SW-IF-USER-PLACED-SHIPS                    
              MOVE WS-COMMAREA TO DFHCOMMAREA                           
           ELSE                                                         
              DISPLAY '2033 NOT SO-VALID-USER-SHIPS '                   
      * IF USER PROVIDED INVALID SHIPS THEN WE WILL PLACE A CURSOR      
      * AT THE FIRST FIELD OF THE USER BOARD ( WE WILL ALSO CLEAR A     
      * BOARD  SO THE USER WILL HAVE TO PROVIDE SHIP DATA ONCE AGAIN)   
              DISPLAY 'SHIPS ARE  INVALID '                             
              PERFORM 2154-INITIALIZE-USER-MAP                          
              MOVE -1 TO POLEUL(1)                                      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2034-MOVE-USER-BOARD-TO-PROG                  
      * PARAGRAPH WILL MOVE USER SHIPS DATA INTO PROGRAM DATA           
      ******************************************************************
       2034-MOVE-USER-BOARD-TO-PROG.                                    
           DISPLAY '2034-MOVE-USER-BOARD-TO-PROG PERFORMED'             
           PERFORM VARYING WS-ITER2 FROM 1 BY 1 UNTIL WS-ITER2 > 10     
                                                                        
                                                                        
              INITIALIZE WS-SCREEN-TABLE(WS-ITER2)                      
              INITIALIZE WS-SCREEN-TABLE2(WS-ITER2)                     
              MOVE       POLEUI(WS-ITER2) TO WS-SCREEN-TABLE(WS-ITER2)  
              DISPLAY 'WS-SCREEN-TABLE ' WS-SCREEN-TABLE(WS-ITER2)      
              DISPLAY ' WS-SCREEN-TABLE2: ' WS-SCREEN-TABLE(WS-ITER2)   
                                                                        
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2035-CHECK-THE-SHIPS                          
      * PROGRAM WILL USE THE SAME FLAGS AND VARIABLES THAT              
      * WERE PREVIOUSLY USED WHLIE PROGRAM WAS GENERATING COMPUTER'S    
      * BOARD                                                           
      ******************************************************************
       2035-CHECK-THE-SHIPS.                                            
           DISPLAY '2035-CHECK-THE-SHIPS PERFORMED '                    
           PERFORM 2036-INITALIZE-SHIP-DATA                             
           PERFORM 2041-INITIALIZE-SCREEN-VARS                          
      * PARAGRAPH WILL CHECK IF USER PROVIDED ONLY VALID CHARACTERS     
      * ONLY LETTER "S" - BIG LETTER S                                  
           PERFORM 2169-CHECK-IF-VALID-CHARACTERS                       
           PERFORM VARYING WS-ITER-ROW FROM 1 BY 1 UNTIL                
                WS-ITER-ROW > 10  OR SO-INVALID-USER-SHIPS              
              PERFORM VARYING WS-ITER-COLUMN FROM 1 BY 1 UNTIL          
                WS-ITER-COLUMN > 10  OR  SO-INVALID-USER-SHIPS          
                                                                        
                  PERFORM 2037-CHECK-THE-BOARD-FIELD                    
              END-PERFORM                                               
           END-PERFORM                                                  
      * IF PROGRAM DIDNT END WITH ERROR ALREADY WE WILL MAKE THAT       
      * ADDITIONAL CHECK                                                
           IF NOT SO-INVALID-USER-SHIPS THEN                            
             DISPLAY '2035 NOT SO-INVALID-USER-SHIPS '                  
             PERFORM 2044-CHECK-IF-ALL-SHIPS-PLACED                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2036-INITALIZE-SHIP-DATA                       
      * IN THIS PARAGRAPH DATA IN INDEX IS NUMBER OF THE FIELDS THAT    
      * SHIP CAN HAVE                                                   
      *                                                                 
      * SO FLAG  SO-THIS-TYPE-NOT-PLACED(5) MEANS THAT                  
      * 5 FIELDS SHIP WAS NOT PLACED     ETC.                           
      *                                                                 
      ******************************************************************
       2036-INITALIZE-SHIP-DATA.                                        
           DISPLAY '2036 PERFORMED '                                    
           SET  SO-THIS-TYPE-NOT-PLACED(5)  TO TRUE                     
           SET  SO-THIS-TYPE-NOT-PLACED(4)  TO TRUE                     
           SET  SO-THIS-TYPE-NOT-PLACED(3)  TO TRUE                     
           SET  SO-THIS-TYPE-NOT-PLACED(2)  TO TRUE                     
           SET  SO-NOT-ALL-SHIPS-ARE-PLACED TO TRUE                     
           SET  SO-SHIP-IS-NOT-VERTICAL     TO TRUE                     
           SET  SO-SHIP-IS-NOT-HORIZONTAL   TO TRUE                     
           SET  SO-VALID-USER-SHIPS         TO TRUE                     
           MOVE 0  TO WS-THIS-TYPE-SHIP-COUNTER(5)                      
           MOVE 0  TO WS-THIS-TYPE-SHIP-COUNTER(4)                      
           MOVE 0  TO WS-THIS-TYPE-SHIP-COUNTER(3)                      
           MOVE 0  TO WS-THIS-TYPE-SHIP-COUNTER(2)                      
                                                                        
      * TEST                                                            
           DISPLAY 'SW-IF-SHIP-PLACED(2)  ' SW-IF-SHIP-PLACED(2)        
           DISPLAY 'SW-IF-SHIP-PLACED(3)  ' SW-IF-SHIP-PLACED(3)        
           DISPLAY 'SW-IF-SHIP-PLACED(4)  ' SW-IF-SHIP-PLACED(4)        
           DISPLAY 'SW-IF-SHIP-PLACED(5)  ' SW-IF-SHIP-PLACED(5)        
           DISPLAY  ' SW-IF-ALL-SHIPS-ARE-PLACED  '                     
                        SW-IF-ALL-SHIPS-ARE-PLACED                      
           DISPLAY 'SW-IF-SHIP-IS-HORIZONTAL '                          
                       SW-IF-SHIP-IS-HORIZONTAL                         
           DISPLAY 'SW-IF-VALID-USER-SHIPS '                            
                       SW-IF-VALID-USER-SHIPS                           
           DISPLAY  ' WS-THIS-TYPE-SHIP-COUNTER(5) '                    
                       WS-THIS-TYPE-SHIP-COUNTER(5)                     
           DISPLAY  ' WS-THIS-TYPE-SHIP-COUNTER(4) '                    
                       WS-THIS-TYPE-SHIP-COUNTER(4)                     
           DISPLAY  ' WS-THIS-TYPE-SHIP-COUNTER(3) '                    
                       WS-THIS-TYPE-SHIP-COUNTER(3)                     
           DISPLAY  ' WS-THIS-TYPE-SHIP-COUNTER(2) '                    
                       WS-THIS-TYPE-SHIP-COUNTER(2)                     
      * /TEST                                                           
           .                                                            
      ******************************************************************
      *                2037-CHECK-THE-BOARD-FIELD                       
      * IF THIS FIELD IS TAKEN (HAS A LETTER 'S' ) THEN WE WILL         
      * CHECK IF THIS 'S' IS A PART OF A HORIZONTAL OR VERTICAL SHIP    
      ******************************************************************
       2037-CHECK-THE-BOARD-FIELD.                                      
           DISPLAY '2037-CHECK-THE-BOARD-FIELD '                        
           IF WS-SCREEN-TABLE(WS-ITER-ROW)(WS-ITER-COLUMN:1) = 'S' THEN 
               DISPLAY '2037 IN IF STAEMENT '                           
                 PERFORM 2038-CHECK-IF-SHIP-HORIZONTAL                  
                 PERFORM 2039-CHECK-IF-SHIP-VERTICAL                    
           ELSE                                                         
               DISPLAY '2037 NOT IN IF STAEMENT '                       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2038-CHECK-IF-SHIP-HORIZONTAL               
      * IN THE MOMENT THIS PARAGRAPH IS CALLED WE WILL GET              
      * A ROW AND COLUMN NUMBER OF A FIRST FIELD OF A SHIP              
      *                                                                 
      * HERE WE WILL CHECK IF THIS SHIP IS A VALID HORIZONTAL SHIP      
      * IF IT ISNT THEN WE WILL CHECK IF THE SHIP IS VERTICAL           
      *                                                                 
      ******************************************************************
       2038-CHECK-IF-SHIP-HORIZONTAL.                                   
           DISPLAY '2038 CHECK IF HORIZONTAL PERFORMED '                
           SET SO-SHIP-IS-NOT-HORIZONTAL TO TRUE                        
           SET SO-NOT-END-OF-SHIP        TO TRUE                        
           MOVE 1                        TO WS-HOW-MANY-ELEMENTS        
           MOVE WS-ITER-ROW              TO WS-ITER-ROW-TEMP            
           MOVE WS-ITER-COLUMN           TO WS-ITER-COLUMN-TEMP         
      * TEST                                                            
           DISPLAY 'SW-IF-SHIP-IS-HORIZONTAL ' SW-IF-SHIP-IS-HORIZONTAL 
           DISPLAY 'SW-IF-NOT-END-OF-SHIP ' SW-IF-NOT-END-OF-SHIP       
           DISPLAY 'WS-ITER-ROW-TEMP     ' WS-ITER-ROW-TEMP             
           DISPLAY 'WS-ITER-COLUMN-TEMP  ' WS-ITER-COLUMN-TEMP          
           DISPLAY 'WS-ITER-ROW          ' WS-ITER-ROW                  
           DISPLAY 'WS-ITER-COLUMN       ' WS-ITER-COLUMN               
           DISPLAY 'WS-HOW-MANY-ELEMENTS ' WS-HOW-MANY-ELEMENTS         
      * /TEST                                                           
                                                                        
      * PARAGRAPH WILL CHECK IF FIRST FIELD OF THIS SHIP CAN            
      * BE HORIZONTAL                                                   
      * ( WE WILL CHECK IF IT HAS ANY NEIGHBORS AT TOP, LEFT SIDE OR    
      *  AT THE BOTTOM )                                                
      * IF IT HAS NEIGHBOR AT THE LEFT SIDE IT MEANS IT IS INVALID      
      * SHIP AND IF IT HAS NEIGHBORS AT THE TOP OR BOTTOM IT MEANS      
      * THAT THIS CANT BE A VALID HORIZONTAL SHIP ( WE WILL CHECK       
      * IF THIS SHIP IS VERTIACAL)                                      
      *                                                                 
      * IF THIS SHIP HAS NEIGHBOR AT THE LEFT IT IS INVALID BECAUSE     
      * OF THE FACT THAT WE SHOULD ALWAYS START THIS PARAGRAPH          
      * FROM THE START OF THE SHIP (SO WE HAVE NEIGHBORS ONLY AT THE    
      * RIGHT SIDE IF IT IS VALID HORIZONTAL SHIP) IF THERE IS  SOMETHIN
      * AT THE LEFT IT CANNOT BE VALID SHIP                             
      *                                                                 
           PERFORM 2051-FIRST-HORIZONTAL-FIELD                          
           IF WS-ITER-COLUMN-TEMP < 10 AND SO-SHIP-IS-HORIZONTAL        
              AND NOT SO-INVALID-USER-SHIPS THEN                        
              DISPLAY ' IN IF STATEMENT AFTER 2051 '                    
              PERFORM 2162-CHECK-HORIZONTAL-FIELDS                    
           ELSE                                                       
      * THIS SHIP IS INVALID (THERE IS ONLY ONE FIELD AND             
      * WE CANNOT PLACE ANYTHING HORIZONTALY HERE (THERE IS A WALL)   
              DISPLAY 'NOT IN IF STATEMENT AFTER 2051 '               
              SET SO-SHIP-IS-NOT-HORIZONTAL TO TRUE                   
              DISPLAY '  SW-IF-SHIP-IS-HORIZONTAL '                   
                       SW-IF-SHIP-IS-HORIZONTAL                       
           END-IF                                                     
                                                                      
           EVALUATE TRUE                                              
      * IF OUR LOOP WENT TO THE END OF THE SHIP AND THERE IS          
      * ONLY ONE FIELD OF THIS SHIP IT CANT BE A VALID HORIZONTAL     
      * SHIP                                                          
           WHEN SO-END-OF-SHIP AND  WS-HOW-MANY-ELEMENTS = 1          
               DISPLAY ' 2038 NOT HORIZONTAL '                        
               SET SO-SHIP-IS-NOT-HORIZONTAL TO TRUE                  
                                                                      
      * IF OUR LOOP WENT TO THE END OF THE SHIP AND THERE IS          
      * ONLY MORE THAN ONE FIELD IT IS A HORIZONTAL SHIP              
               DISPLAY '2038 SW-SHIP-IS-HORIZONTAL '                  
                       SW-IF-SHIP-IS-HORIZONTAL                       
                                                                      
           WHEN SO-END-OF-SHIP AND WS-HOW-MANY-ELEMENTS > 1           
               SET SO-SHIP-IS-HORIZONTAL     TO TRUE                  
               DISPLAY '2038 HORIZONTAL '                             
               DISPLAY ' 2038 SW-IF-SHIP-IS-HORIZONTAL '              
                       SW-IF-SHIP-IS-HORIZONTAL                       
      * THERE IS NO OTHER VALID CASE HERE                             
           WHEN OTHER                                                 
               DISPLAY '2038 WHEN OTHER. NOT HORIZONATAL '            
               SET SO-SHIP-IS-NOT-HORIZONTAL TO TRUE                  
               DISPLAY '2038 SW-IF-SHIP-IS-HORIZONTAL '               
                       SW-IF-SHIP-IS-HORIZONTAL                       
           END-EVALUATE                                               
           IF SO-SHIP-IS-HORIZONTAL AND NOT SO-INVALID-USER-SHIPS THEN  
              DISPLAY 'SO-SHIP-IS-HORIZONTAL AND NOT SO-INVALID-USER-  '
                            'SHIP '                                     
              PERFORM 2040-MOVE-HORIZONTAL-SHIP-DATA                    
              PERFORM 2042-SET-WHAT-SHIP-WAS-ADDED                      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2039-CHECK-IF-SHIP-VERTICAL                    
      * PARAGRAPH WILL BE CALLED IN TWO SCENARIOS                       
      * 1. PROGRAM ALREADY FINDED OUT THAT SHIP WE ARE CHECKING IS      
      * VALID HORIZONTAL SHIP                                           
      * 2. PROGRAM LAREADY FINDED OUT THAT THIS SHIP IS INVALID         
      * HORIZONTAL SHIP                                                 
      *                                                                 
      * IN CASE OPTION 2 IS TRUE WE WILL CHECK IF THIS SHIP IS VERTICAL 
      ******************************************************************
       2039-CHECK-IF-SHIP-VERTICAL.                                     
           DISPLAY '2039 CHECK IF VERTICAL PERFORMED '                  
           IF  SO-SHIP-IS-NOT-HORIZONTAL AND                            
              NOT SO-INVALID-USER-SHIPS  THEN                           
              DISPLAY '2039 SO-SHIP-IS-NOT-HORIZONTAL AND NOT '         
                          'SO-INVALID-USER-SHIPS '                      
                                                                        
              SET SO-SHIP-IS-NOT-VERTICAL TO TRUE                       
              SET SO-NOT-END-OF-SHIP     TO TRUE                        
              MOVE 1 TO WS-HOW-MANY-ELEMENTS                            
      * PARAGRAPH WILL CHECK FIRST FIELD OF THIS SHIP                   
      * IF IT DOES HAVE ANY NEIGHBOR EXECPT FOR THE ONE UNDER IT        
      * WE WILL DROP THE PROCESSING ( BECAUSE IT IS INVALID)            
              MOVE WS-ITER-ROW    TO WS-ITER-ROW-TEMP                   
              MOVE WS-ITER-COLUMN TO WS-ITER-COLUMN-TEMP                
                                                                        
      * TEST                                                            
              DISPLAY  'SW-IF-SHIP-IS-VERTICAL  ' SW-IF-SHIP-IS-VERTICAL
              DISPLAY  'SW-IF-NOT-END-OF-SHIP  ' SW-IF-NOT-END-OF-SHIP  
              DISPLAY  'WS-HOW-MANY-ELEMENTS '   WS-HOW-MANY-ELEMENTS   
              DISPLAY  'WS-ITER-ROW          '   WS-ITER-ROW            
              DISPLAY  'WS-ITER-COLUMN       '   WS-ITER-COLUMN         
              DISPLAY  'WS-ITER-ROW-TEMP     '   WS-ITER-ROW-TEMP       
              DISPLAY  'WS-ITER-COLUMN-TEMP  '   WS-ITER-COLUMN-TEMP    
      */TEST                                                            
              PERFORM 2054-FIRST-VERTICAL-FIELD                         
              PERFORM 2163-CHECK-VERTICAL-FIELDS                        
                                                                        
              EVALUATE TRUE                                             
      * IF PROGRAM GET TO THE END OF THE SHIP AND THERE IS ONLY         
      * ONE ELEMENT IT MEANS THAT IT IS INVALID                         
               WHEN SO-END-OF-SHIP AND  WS-HOW-MANY-ELEMENTS = 1        
                 DISPLAY '2039 END OF SHIP ELEMENTS = 1'                
                 SET SO-SHIP-IS-NOT-VERTICAL TO TRUE                    
                 SET SO-INVALID-USER-SHIPS   TO TRUE                    
                 DISPLAY  'SW-IF-SHIP-IS-VERTICAL  '                    
                            SW-IF-SHIP-IS-VERTICAL                      
                 DISPLAY  'SW-IF-VALID-USER-SHIPS '                     
                             SW-IF-VALID-USER-SHIPS                     
                 MOVE 'INVALID SHIPS ' TO MSGO                          
                  DISPLAY 'MSGO: ' MSGO                                 
                 PERFORM 2100-SEND-THE-MAP                              
               WHEN SO-END-OF-SHIP AND WS-HOW-MANY-ELEMENTS > 1         
                 DISPLAY '2039 END OF SHIP ELEMENTS > 1'                
                 SET SO-SHIP-IS-VERTICAL       TO TRUE                  
                 DISPLAY  'SW-IF-SHIP-IS-VERTICAL  '                    
                            SW-IF-SHIP-IS-VERTICAL                      
               WHEN OTHER                                               
                 DISPLAY ' 2039 EVALUATE WHEN OTHER NO ACTION '         
              END-EVALUATE                                              
                                                                        
              IF SO-SHIP-IS-VERTICAL AND NOT SO-INVALID-USER-SHIPS THEN 
                 DISPLAY 'PERFORM 2039 MOVE VERTICAL SHIP '             
                 PERFORM 2043-MOVE-VERTICAL-SHIP-DATA                   
                 PERFORM 2042-SET-WHAT-SHIP-WAS-ADDED                   
              END-IF                                                  
           ELSE                                                       
              DISPLAY '2039 HORIZONTAL NO ACTION '                    
           END-IF                                                     
           .                                                          
      ****************************************************************
      *                2040-MOVE-HORIZONTAL-SHIP-DATA                 
      * PARAGRAPH WILL MARK ALL FIELDS OF THIS SHIP ON PROGRAM ARRAY  
      * AS 'X' - > PROGRAM WILL TREAT IT LIKE THERE IS NOTHING THERE  
      *                                                               
      * AND WE WILL MOVE THIS SHIP DATA INTO MAP VARIABLES            
      ****************************************************************
       2040-MOVE-HORIZONTAL-SHIP-DATA.                                
           DISPLAY '2040-MOVE-HORIZONTAL-SHIP-DATA PERFORMED'         
           MOVE WS-ITER-ROW    TO WS-ITER-ROW-TEMP                    
           MOVE WS-ITER-COLUMN TO WS-ITER-COLUMN-TEMP                 
                                                                      
           DISPLAY  'WS-ITER-ROW         ' WS-ITER-ROW                
           DISPLAY  'WS-ITER-COLUMN      ' WS-ITER-COLUMN             
           DISPLAY  'WS-ITER-ROW-TEMP    ' WS-ITER-ROW-TEMP           
           DISPLAY  'WS-ITER-COLUMN-TEMP ' WS-ITER-COLUMN-TEMP        
           PERFORM WS-HOW-MANY-ELEMENTS TIMES                         
            MOVE CT-TAKEN-SPOT TO WS-SCREEN-TABLE(WS-ITER-ROW-TEMP)(  
                                  WS-ITER-COLUMN-TEMP:1)              
            MOVE CT-SHIP-FIELD TO WS-SCREEN-TABLE2(WS-ITER-ROW-TEMP)( 
                                  WS-ITER-COLUMN-TEMP:1)              
            ADD 1              TO WS-ITER-COLUMN-TEMP                 
            DISPLAY 'WS-ITER-COLUMN-TEMP: ' WS-HOW-MANY-ELEMENTS      
           END-PERFORM                                                
      * TEST                                                          
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10   
              DISPLAY 'WS-SCREEN-TABLE: ' WS-SCREEN-TABLE(WS-ITER5)   
           END-PERFORM                                                
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10   
              DISPLAY 'WS-SCREEN-TABLE2: ' WS-SCREEN-TABLE2(WS-ITER5) 
           END-PERFORM                                                
      */ TEST                                                           
           .                                                            
      ******************************************************************
      *                      2041-INITIALIZE-SCREEN-VARS                
      ******************************************************************
       2041-INITIALIZE-SCREEN-VARS.                                     
           DISPLAY '2041-INITIALIZE-SCREEN-VARS PERFORMED'              
           PERFORM VARYING WS-ITER4 FROM 1 BY 1 UNTIL WS-ITER4 > 10     
              MOVE SPACE TO POLEUO(WS-ITER4)                            
              DISPLAY '2041: ' POLEUO(WS-ITER4)                         
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2042-SET-WHAT-SHIP-WAS-ADDED                   
      * THIS PARAGRAPH WILL EVALUATE THROUGH AMOUNT OF FIELDS THAT      
      * USER'S SHIP HAD,                                                
      * USER CAN HAVE LIMITED AMOUNT OF EACH OF THE SHIPS               
      * SO THIS PARAGRAPH WILL CHECK IF USER ADDED VALID NUMBER         
      * OF SHIPS                                                        
      ******************************************************************
       2042-SET-WHAT-SHIP-WAS-ADDED.                                    
           DISPLAY '2042-SET-WHAT-SHIP-WAS-ADDED PERFORMED '            
      * EACH TYPE OF THE SHIP HAS OTHER                                 
      * MAXIMAL NUMBER ON THE SCREEN THIS                               
      * PARAGRAPH WILL CALCULATE THOSE VARIABLES                        
                                                                        
              ADD 1 TO WS-THIS-TYPE-SHIP-COUNTER(WS-HOW-MANY-ELEMENTS)  
              EVALUATE TRUE                                             
              WHEN WS-THIS-TYPE-SHIP-COUNTER(WS-HOW-MANY-ELEMENTS)      
                  < WS-MAXIMAL-AMOUNT-OF-SHIPS(WS-HOW-MANY-ELEMENTS)    
              OR  WS-THIS-TYPE-SHIP-COUNTER(WS-HOW-MANY-ELEMENTS)       
                  = WS-MAXIMAL-AMOUNT-OF-SHIPS(WS-HOW-MANY-ELEMENTS)    
                 DISPLAY '2042-IN WHEN STATEMENT '                      
                 SET SO-THIS-TYPE-SHIP-PLACED(WS-HOW-MANY-ELEMENTS)     
                                                                 TO TRUE
                DISPLAY 'SW-IF-SHIP-PLACED(WS-HOW-MANY-ELEMENTS) '      
                          SW-IF-SHIP-PLACED(WS-HOW-MANY-ELEMENTS)       
              WHEN OTHER                                                
                 SET SO-INVALID-USER-SHIPS TO TRUE                      
                 MOVE 'INVALID SHIPS ' TO MSGO                          
                 DISPLAY 'SW-IF-VALID-USER-SHIPS  '                     
                                 SW-IF-VALID-USER-SHIPS                 
                 PERFORM 2100-SEND-THE-MAP                              
              END-EVALUATE                                              
                                                                        
           DISPLAY '2024 ENDS'                                          
           .                                                            
      ******************************************************************
      *                  2043-MOVE-VERTICAL-SHIP-DATA                   
      * PARAGRAPH WILL BE USED TO MARK OUR VALID SHIP WITH 'X' SYMBOL   
      * ( PROGRAM WILL TREAT THOSE FIELDS AS EMPTY)                     
      *                                                                 
      * PARAGRAPH WILL ALSO CHECK THIS SHIP ON THE ARRAY THAT WILL      
      * BE LATER USED IN ORDER TO DISPLAY DATA ON THE SCREEN            
      ******************************************************************
       2043-MOVE-VERTICAL-SHIP-DATA.                                    
           DISPLAY ' 2043-MOVE-VERTICAL-SHIP-DATA  PERFORMED'           
                                                                        
           MOVE WS-ITER-ROW     TO WS-ITER-ROW-TEMP                     
           MOVE WS-ITER-COLUMN  TO WS-ITER-COLUMN-TEMP                  
                                                                        
           PERFORM WS-HOW-MANY-ELEMENTS TIMES                           
            MOVE 'X' TO WS-SCREEN-TABLE(WS-ITER-ROW-TEMP)(              
                               WS-ITER-COLUMN-TEMP:1)                   
            MOVE 'S' TO WS-SCREEN-TABLE2(WS-ITER-ROW-TEMP)(             
                              WS-ITER-COLUMN-TEMP:1)                    
            ADD 1 TO WS-ITER-ROW-TEMP                                   
                                                                        
           END-PERFORM                                                  
      *TEST                                                             
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
                 DISPLAY 'WS-SCREEN-TABLE (' WS-ITER5 ')'               
                                 WS-SCREEN-TABLE(WS-ITER5)              
                 DISPLAY 'WS-SCREEN-TABLE2 (' WS-ITER5 ')'              
                                 WS-SCREEN-TABLE2(WS-ITER5)             
           END-PERFORM                                                  
      */TEST                                                            
           .                                                            
      ******************************************************************
      *                  2044-CHECK-IF-ALL-SHIPS-PLACED                 
      *                                                                 
      * PARAGRAPH WILL CHECK IF ALL SHIPS WERE PLACED ON THE BOARD      
      *                                                                 
      * IF STATEMENT BELOW WILL CHECK IF                                
      *                                                                 
      *    ACTUAL NUMBER OF (TYPE OF SHIP) SHIP IS EQUAL TO             
      *  MAXIMAL NUMBER OF (TYPE OF SHIP) SHIP                          
      *                                                                 
      *                                                                 
      *  WHERE TYPE OF SHIP CAN BE                                      
      *                                                                 
      *      5   - SHIP WITH THE LENGTH OF 5                            
      *      4   - SHIP WITH THE LENGTH OF 4                            
      *      3   - SHIP WITH THE LENGTH OF 3                            
      *      2   - SHIP WITH THE LENGTH OF 2                            
      *                                                                 
      ******************************************************************
       2044-CHECK-IF-ALL-SHIPS-PLACED.                                  
           DISPLAY '2044-CHECK-IF-ALL-SHIPS-PLACED PERFOREMD '          
           IF                                                           
              WS-THIS-TYPE-SHIP-COUNTER(5) =                            
              WS-MAXIMAL-AMOUNT-OF-SHIPS(5)  AND                        
              WS-THIS-TYPE-SHIP-COUNTER(4) =                            
              WS-MAXIMAL-AMOUNT-OF-SHIPS(4)  AND                        
              WS-THIS-TYPE-SHIP-COUNTER(3) =                            
              WS-MAXIMAL-AMOUNT-OF-SHIPS(3)  AND                        
              WS-THIS-TYPE-SHIP-COUNTER(2) =                            
              WS-MAXIMAL-AMOUNT-OF-SHIPS(2)  THEN                       
                                                                        
              SET SO-VALID-USER-SHIPS TO TRUE                           
      * TEST                                                            
              DISPLAY 'SO-VALID-USER-SHIPS (ALL ARE PLACED '            
              DISPLAY 'SW-IF-VALID-USER-SHIPS'                          
                         SW-IF-VALID-USER-SHIPS                         
      * /TEST                                                           
                                                                        
           ELSE                                                         
             DISPLAY '5FIELDS COUNTER: '  WS-THIS-TYPE-SHIP-COUNTER(5)  
             DISPLAY '4FIELDS COUNTER: '  WS-THIS-TYPE-SHIP-COUNTER(4)  
             DISPLAY '3FIELDS COUNTER: '  WS-THIS-TYPE-SHIP-COUNTER(3)  
             DISPLAY '2FIELDS COUNTER: '  WS-THIS-TYPE-SHIP-COUNTER(2)  
             MOVE 'INVALID SHIPS                    ' TO MSGO           
             SET SO-INVALID-USER-SHIPS TO TRUE                          
             PERFORM 2100-SEND-THE-MAP                                  
      * TEST                                                            
             DISPLAY 'NOT ALL SHIPS WERE PLACED '                       
             DISPLAY 'SW-IF-VALID-USER-SHIPS'                           
                         SW-IF-VALID-USER-SHIPS                         
      * /TEST                                                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2051-FIRST-HORIZONTAL-FIELD                    
      * THIS PARAGRAPH WILL BE CALLED IN ORDER TO DETERMINE IF          
      * THIS SHIP CAN BE A VALID HORIZONTAL SHIP                        
      * WE GOT X AND Y OF THE FIRST FIELD OF THAT SHIP WE HAVE TO       
      * VALIDTE ITS NEIGHBORS                                           
      *                                                                 
      *  WE WILL HAVE TO CHECK IF THIS FIELD DON'T HAVE A LEFT, TOP     
      * AND BOTTOM NEIGHBOR , IF IT HAVE ANY OF THOSE THEN IT IS INVALID
      * SHIP                                                            
      ******************************************************************
       2051-FIRST-HORIZONTAL-FIELD.                                     
           DISPLAY '2051-FIRST-HORIZONTAL-FIELD PERFORMED '             
           MOVE WS-ITER-ROW    TO WS-ROW-VALIDATION                    
           MOVE WS-ITER-COLUMN TO WS-COLUMN-VALIDATION                 
      * IF WE ARE AT THE LEFT SIDE OF THE BOARD WE CANT HAVE LEFT      
      * NEIGHBOR                                                       
           SET SO-SHIP-POSITION-VALID TO TRUE                          
           SET SO-SHIP-IS-HORIZONTAL TO TRUE                           
      * TEST                                                           
           DISPLAY 'WS-ITER-ROW '          WS-ITER-ROW                 
           DISPLAY 'WS-ITER-COLUMN '       WS-ITER-COLUMN              
           DISPLAY 'WS-ROW-VALIDATION '    WS-ROW-VALIDATION           
           DISPLAY 'WS-COLUMN-VALIDATION ' WS-COLUMN-VALIDATION        
           DISPLAY 'SW-IF-SHIP-POSTION-VALID   '                       
                    SW-IF-SHIP-POSTION-VALID                           
           DISPLAY 'SW-IF-SHIP-IS-HORIZONTAL '                         
                   SW-IF-SHIP-IS-HORIZONTAL                            
      * /TEST                                                          
           IF WS-COLUMN-VALIDATION  = 1 THEN                           
               DISPLAY '2051 WS-COLUMN-VALIDATION  = 1 NO ACTION '     
           ELSE                                                        
               DISPLAY '2051 WS-COLUMN-VALIDATION NOT = 1  '           
               PERFORM 2026-CHECK-LEFT-NEIGHBOR                        
                                                                       
                                                                       
                                                                       
               IF SO-SHIP-POSITION-INVALID THEN                        
                  DISPLAY '2051 AFTER 2026 SO-SHIP-POSITION-INVALID'   
                                                                       
                  SET SO-INVALID-USER-SHIPS TO TRUE                    
                  MOVE 'INVALID SHIPS   ' TO MSGO                      
                  DISPLAY 'MSGO : '  MSGO                              
                  DISPLAY 'SW-IF-VALID-USER-SHIPS   '                  
                          SW-IF-VALID-USER-SHIPS                       
                  PERFORM 2100-SEND-THE-MAP                            
               END-IF                                                  
           END-IF                                                      
      * IF WE ARE AT THE TOP OF THE BOARD WE CANT HAVE UPPER           
      * NEIGHBOR                                                        
           IF WS-ROW-VALIDATION  = 1 THEN                               
               DISPLAY '2051 WS-ROW-VALIDATION =  1  NO ACTION '        
           ELSE                                                         
               DISPLAY '2051 WS-ROW-VALIDATION NOT =  1  NO ACTION '    
               PERFORM 2028-CHECK-TOP-NEIGHBOR                          
           END-IF                                                       
      * IF WE ARE AT THE TOP OF THE BOARD WE CANT HAVE BOTTOM           
      * NEIGHBOR                                                        
           IF WS-ROW-VALIDATION  = 10 THEN                              
               DISPLAY '2051 WS-ROW-VALIDATION  = 10 NO ACTION '        
           ELSE                                                         
               DISPLAY '2051 WS-ROW-VALIDATION NOT = 10  '              
               PERFORM 2029-CHECK-BOTTOM-NEIGHBOR                       
           END-IF                                                       
           IF SO-SHIP-POSITION-INVALID THEN                             
                DISPLAY '2051 SO-SHIP-POSITION INVALID '                
                SET SO-SHIP-IS-NOT-HORIZONTAL  TO TRUE                  
                DISPLAY 'SW-IF-SHIP-IS-HORIZONTAL '                     
                         SW-IF-SHIP-IS-HORIZONTAL                       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2052-CHECK-MIDDLE-HORIZONTAL                  
      * PARAGRAPH WILL CHECK IF A GIVEN FIELD HAS                       
      * NEIGHBORS AT THE TOP OR AT THE BOTTOM                           
      *                                                                 
      * IF ANY OF THIS TWO NEIGHBORS EXISTS  FLAG                       
      * SO-SHIP-POSITION-INVALID WILL BE SET TO TRUE                    
      ******************************************************************
       2052-CHECK-MIDDLE-HORIZONTAL.                                    
           DISPLAY '2052-CHECK-MIDDLE-HORIZONTAL IS PERFORMED'          
           MOVE WS-ITER-ROW         TO WS-ROW-VALIDATION                
           MOVE WS-ITER-COLUMN-TEMP TO WS-COLUMN-VALIDATION             
           SET SO-SHIP-POSITION-VALID TO TRUE                           
           DISPLAY '2052 WS-ITER-ROW                   '                
                         WS-ITER-ROW                                    
           DISPLAY '2052 WS-ITER-COLUMN-TEMP           '                
                         WS-ITER-COLUMN-TEMP                            
           DISPLAY '2052 WS-ROW-VALIDATION             '                
                         WS-ROW-VALIDATION                              
           DISPLAY '2052 WS-COLUMN-VALIDATION          '                
                         WS-COLUMN-VALIDATION                           
           DISPLAY '2052  SW-IF-SHIP-POSTION-VALID     '                
                          SW-IF-SHIP-POSTION-VALID                      
           IF WS-ROW-VALIDATION = 1 THEN                                
              DISPLAY '2052 WS-ROW-VALIDATION = 1'                      
              DISPLAY 'NO ACTION IS TAKEN '                             
      *     CONTINUE                                                    
           ELSE                                                         
              DISPLAY '2052 WS-ROW-VALIDATION != 1'                     
               PERFORM 2028-CHECK-TOP-NEIGHBOR                          
           END-IF                                                       
           IF WS-ROW-VALIDATION = 10 THEN                               
              DISPLAY '2052 WS-ROW-VALIDATION = 10'                     
              DISPLAY '2052 NO ACTION '                                 
      *      CONTINUE                                                   
           ELSE                                                         
              DISPLAY '2052 WS-ROW-VALIDATION != 10'                    
               PERFORM 2029-CHECK-BOTTOM-NEIGHBOR                       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2053-CHECK-FOR-LAST-HORIZONTAL                
      * THIS PARAGRAPH WILL BE PREFORMED FOR THE                        
      * LAST FIELD OF THE HORIZONTAL FLIGHT FOR EXMAPLE:                
      * ---X WHERE '-' REPRESENTS OTHER (THIS SHIP) FIELDS AND          
      * 'X' REPRESENT LAST FIELD IN THIS SHIP,                          
      * HERE WE WILL CHECK IF THIS LAST FIELD HAS RIGHT NEIGHBOR        
      * OR NOT                                                          
      ******************************************************************
       2053-CHECK-FOR-LAST-HORIZONTAL.                                  
           DISPLAY '2053-CHECK-FOR-LAST-HORIZONTAL  PERFORMED'          
           SUBTRACT 1 FROM WS-ITER-COLUMN-TEMP                          
           MOVE WS-ITER-COLUMN-TEMP TO WS-COLUMN-VALIDATION             
           MOVE WS-ITER-ROW         TO WS-ROW-VALIDATION                
           DISPLAY '2053:    WS-ITER-COLUMN-TEMP   '                    
                            WS-ITER-COLUMN-TEMP                         
           DISPLAY '2053:    WS-ITER-ROW           '                    
                             WS-ITER-ROW                                
           DISPLAY '2053:    WS-COLUMN-VALIDATION  '                    
                             WS-COLUMN-VALIDATION                       
           DISPLAY '2053:    WS-ROW-VALIDATION     '                    
                             WS-ROW-VALIDATION                          
           DISPLAY '2053:    WS-ITER-COLUMN-TEMP   '                    
                             WS-ITER-COLUMN-TEMP                        
           IF WS-COLUMN-VALIDATION = 10                                 
              DISPLAY '2053 WS-COLUMN-VALIDTION = 10 '                  
              SET SO-SHIP-POSITION-VALID TO TRUE                        
              DISPLAY  '2053 SW-IF-SHIP-POSTION-VALID: '                
                       SW-IF-SHIP-POSTION-VALID                         
           ELSE                                                         
              DISPLAY '2053 WS-COLUMN-VALIDATION != 10 '                
              PERFORM 2027-CHECK-RIGHT-NEIGHBOR                         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                2054-FIRST-VERTICAL-FIELD                        
      ******************************************************************
       2054-FIRST-VERTICAL-FIELD.                                       
           DISPLAY '2054-FIRST-VERTICAL-FIELD PERFORMED '               
           MOVE WS-ITER-ROW-TEMP       TO WS-ROW-VALIDATION             
           MOVE WS-ITER-COLUMN-TEMP    TO WS-COLUMN-VALIDATION          
           SET  SO-SHIP-POSITION-VALID TO TRUE                          
           SET  SO-SHIP-IS-VERTICAL    TO TRUE                          
                                                                        
      * TEST                                                            
           DISPLAY 'WS-ITER-ROW-TEMP     '          WS-ITER-ROW-TEMP    
           DISPLAY 'WS-ITER-COLUMN-TEMP  '          WS-ITER-COLUMN-TEMP 
           DISPLAY 'WS-ROW-VALIDATION    '          WS-ROW-VALIDATION   
           DISPLAY 'WS-COLUMN-VALIDATION '          WS-COLUMN-VALIDATION
           DISPLAY 'SW-IF-SHIP-POSTION-VALID   '                        
                            SW-IF-SHIP-POSTION-VALID                    
           DISPLAY ' SW-IF-SHIP-IS-VERTICAL   '                         
                    SW-IF-SHIP-IS-VERTICAL                              
      * /TEST                                                           
           IF WS-COLUMN-VALIDATION = 10                                 
              DISPLAY '2054 WS-COLUMN-VALIDATION = 10  NO ACTION  '     
           ELSE                                                         
              DISPLAY '2054 WS-COLUMN-VALIDATION NOT = 10    '          
              PERFORM 2027-CHECK-RIGHT-NEIGHBOR                         
           END-IF                                                       
                                                                        
           IF WS-COLUMN-VALIDATION = 1                                  
              DISPLAY '2054 WS-COLUMN-VALIDATION = 1 NO ACTION   '      
           ELSE                                                         
              DISPLAY '2054 WS-COLUMN-VALIDATION NOT = 1    '           
              PERFORM 2026-CHECK-LEFT-NEIGHBOR                          
           END-IF                                                       
                                                                        
           IF WS-ROW-VALIDATION = 1                                     
              DISPLAY '2054 WS-ROW-VALIDATION =  1  NO ACTION   '       
           ELSE                                                         
              DISPLAY '2054 WS-ROW-VALIDATION NOT = 1  '                
              PERFORM 2028-CHECK-TOP-NEIGHBOR                           
           END-IF                                                       
                                                                        
           IF SO-SHIP-POSITION-INVALID THEN                             
              DISPLAY '2054 SO-SHIP-POSITION-INVALID (IF STATEMENT) '   
              SET SO-INVALID-USER-SHIPS TO TRUE                         
              MOVE 'INVALID SHIPS ' TO MSGO                             
              DISPLAY 'MSGO: '  MSGO                                    
              DISPLAY 'SW-IF-VALID-USER-SHIPS '                         
                       SW-IF-VALID-USER-SHIPS
              PERFORM 2100-SEND-THE-MAP                                 
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2055-CHECK-MIDDLE-VERTICAL                   
      * THIS PARAGRAPH WILL CHECK IF OUF FIELD HAS NEIGHBROS AT         
      * THE LEFT OR RIGHT SIDE                                          
      * IF IT DOES IT WILL BE TREATED AS INVALID SHIP                   
      ******************************************************************
       2055-CHECK-MIDDLE-VERTICAL.                                      
           DISPLAY ' 2055-CHECK-MIDDLE-VERTICAL PERFORMED'              
           IF WS-COLUMN-VALIDATION = 10                                 
      * IF IT IS IMPOSSIBLE TO HAVE THE RIGHT NEIGHBOUR THEN CONTINUE   
      *       CONTINUE                                                  
              DISPLAY '2055 WS-COLUMN-VALIDATION = 10   NO ACTION'      
           ELSE                                                         
              DISPLAY '2055 WS-COLUMN-VALIDATION NOT = 10  '            
              PERFORM 2027-CHECK-RIGHT-NEIGHBOR                         
           END-IF                                                       
                                                                        
           IF WS-COLUMN-VALIDATION = 1                                  
      * IF IT IS IMPOSSIBLE TO HAVE LEFT NEIGHBOURS                     
      *       CONTINUE                                                  
              DISPLAY '2055 WS-COLUMN-VALIDATION  = 1  NO ACTION'       
           ELSE                                                         
              DISPLAY '2055 WS-COLUMN-VALIDATION NOT = 1  '             
              PERFORM 2026-CHECK-LEFT-NEIGHBOR                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2056-CHECK-LAST-VERTICAL                        
      * PARAGRAPH WILL BE PERFORMED WHILE WE WILL BE CHECKING           
      * THE LAST VERTICAL FIELD ( FIELD THAT IS AT THE BOTTOM OF        
      * THE VERTICAL SHIP)                                              
      *  HERE WE WILL CHECK IF IT IS POSSIBLE THAT OUR FIELD            
      * HAS THE BOTTOM NEIGBHOUR , AND IF IT IS PHYSICLY POSSIBLE THEN  
      * WE WILL CHECK IF THIS IS THE CASE                               
      ******************************************************************
       2056-CHECK-LAST-VERTICAL.                                        
           DISPLAY ' 2056-CHECK-LAST-VERTICAL PERFORMED'                
           IF WS-ROW-VALIDATION  = 10 THEN                              
                DISPLAY 'WS-ROW-VALIDATION =  10 '                      
                SET SO-SHIP-POSITION-VALID TO TRUE                      
                DISPLAY '2056 SW-IF-SHIP-POSTION-VALID   '              
                         SW-IF-SHIP-POSTION-VALID                       
           ELSE                                                         
                DISPLAY 'WS-ROW-VALIDATION NOT = 10 '                   
                PERFORM 2029-CHECK-BOTTOM-NEIGHBOR                      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *              2061-PREPARE-USER-BOARD-DATA                       
      ******************************************************************
       2061-PREPARE-USER-BOARD-DATA.                                    
           DISPLAY '2061-PREPARE-USER-BOARD-DATA PERFORMED'             
                                                                        
           MOVE WS-MINE-NICK             TO T02-FIRST-PLAYER-TEXT       
           MOVE WS-MINE-NICK             TO WS-USER-NICK                
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN         TO T02-FIRST-PLAYER-LEN        
                                                                        
           MOVE WS-ENEMY-NICK            TO T02-SECOND-PLAYER-TEXT      
           MOVE WS-ENEMY-NICK            TO WS-USER-NICK                
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN         TO T02-SECOND-PLAYER-LEN       
      * TEST                                                            
           DISPLAY '2061 WS-MINE-NICK                '                  
                         WS-MINE-NICK                                   
           DISPLAY '2061 T02-FIRST-PLAYER-TEXT       '                  
                         T02-FIRST-PLAYER-TEXT                          
           DISPLAY '2061 WS-USER-NICK                '                  
                         WS-USER-NICK              
           DISPLAY '2061 WS-ENEMY-NICK               '                  
                         WS-ENEMY-NICK                                  
           DISPLAY '2061 T02-SECOND-PLAYER-TEXT      '                  
                         T02-SECOND-PLAYER-TEXT                         
           DISPLAY '2061 WS-USER-NICK                '                  
                         WS-USER-NICK                                   
           DISPLAY '2061 T02-FIRST-PLAYER-LEN        '                  
                         T02-FIRST-PLAYER-LEN                           
           DISPLAY '2061 T02-SECOND-PLAYER-LEN       '                  
                         T02-SECOND-PLAYER-LEN                          
                                                                        
      */TEST                                                            
           .                                                            
      ******************************************************************
      *            2062-PREPARE-ENEMY-BOARD-DATA                        
      ******************************************************************
       2062-PREPARE-ENEMY-BOARD-DATA.                                   
           DISPLAY '2062-PREPARE-ENEMY-BOARD-DATA PERFORMED'            
           MOVE WS-ENEMY-NICK TO T02-FIRST-PLAYER-TEXT                  
           MOVE WS-ENEMY-NICK TO WS-USER-NICK                           
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN TO T02-FIRST-PLAYER-LEN                
                                                                        
           MOVE WS-MINE-NICK TO T02-SECOND-PLAYER-TEXT                  
           MOVE WS-MINE-NICK TO WS-USER-NICK                            
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN TO T02-SECOND-PLAYER-LEN               
      * TEST                                                            
           DISPLAY '2062 WS-MINE-NICK                '                  
                         WS-MINE-NICK                                   
           DISPLAY '2062 T02-FIRST-PLAYER-TEXT       '                  
                         T02-FIRST-PLAYER-TEXT                          
           DISPLAY '2062 WS-USER-NICK                '                  
                         WS-USER-NICK                                   
           DISPLAY '2062 WS-ENEMY-NICK               '                  
                         WS-ENEMY-NICK      
           DISPLAY '2062 T02-SECOND-PLAYER-TEXT      '                  
                         T02-SECOND-PLAYER-TEXT                         
           DISPLAY '2062 WS-USER-NICK                '                  
                         WS-USER-NICK                                   
           DISPLAY '2062 T02-FIRST-PLAYER-LEN        '                  
                         T02-FIRST-PLAYER-LEN                           
           DISPLAY '2062 T02-SECOND-PLAYER-LEN       '                  
                         T02-SECOND-PLAYER-LEN                          
                                                                        
      */TEST                                                            
           .                                                            
      ******************************************************************
      *               2063-MARK-USER-AS-DISCONNECTED                    
      * IF OUR ENEMY IS DISCONNECTED ( MAYBE USER PRESSED 'F3')         
      * OR SERRIOUS ERROR HAPPEND                                       
      *     WE WILL MOVE 'XXXXXX' SYMBOL TO THE PLAYER THAT HAS         
      * THE TURN                                                        
      * THIS WILL BE MESSAGE INDICATING THAT GAME SHOULD END            
      ******************************************************************
       2063-MARK-USER-AS-DISCONNECTED.                                  
           DISPLAY ' 2063-MARK-USER-AS-DISCONNECTED PERFORMED'          
           IF SO-ENEMY-IS-DISCONECTED THEN                              
                DISPLAY '2063 IN IF STATEMENT ( DISCONNECTED )'         
                MOVE  CT-DISCONECTED-USER-SYMBOL TO WS-USER-NAME        
                DISPLAY 'WS-USER-NAME: ' WS-USER-NAME                   
           ELSE                                                         
               DISPLAY '2063 NOT IN IF STATEMENT NO ACTION '            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                2064-MARK-USER-AS-WINNER                         
      * THIS IF STAEMENT WILL BE TRUE IF THIS USER (THIS TRANSACTION    
      * USER) WON THE GAME, TO INDICATE THAT FACT TO THE OTHER PLAYER   
      * WE WILL MODIFY DB2 VARIABLE THAT INDICATE WHO HAS THE TURN      
      * TO 'AAAAAA' THAT WILL INDICATE THAT OPPOSITE PLAYER SHOULD      
      * NOT WAIT FOR ITS TURN (IT WILL NEVER HAPPEN) AND SHOULD      
      * DISPLAY PROPER MESSAGE                                          
      ******************************************************************
       2064-MARK-USER-AS-WINNER.                                        
           DISPLAY '2064-MARK-USER-AS-WINNER PERFORMED '                
           IF SO-THIS-PLAYER-WIN THEN                                   
              DISPLAY 'SO-THIS-PLYAER-WON     '                         
              MOVE    CT-USER-WIN-SYMBOL     TO WS-USER-NAME            
              DISPLAY 'WS-USER-NAME: ' WS-USER-NAME                     
           ELSE                                                         
              DISPLAY '2064 NOT IN IF STATEMENT NO ACTION '             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2065-MARK-USER-AS-INACTIVE                    
      * THIS STATEMENT WILL MOVE 'BBBBBB' SYMBOL THE THE                
      * "PLAYER WITH TURN"                                              
      * THAT WILL INDICATE THAT ENEMY WAS INACTIVE                      
      * (HE DIDNT MAKE A MOVE OR ANYTHING)                              
      ******************************************************************
       2065-MARK-USER-AS-INACTIVE.                                      
           DISPLAY '2065-MARK-USER-AS-INACTIVE PERFORMED'               
           IF SO-ENEMY-IS-INACTIVE THEN                                 
             DISPLAY '2065 SO-ENEMY-IS-INACTIVE  '                      
             MOVE CT-ENEMY-INACTIVE-SYMBOL TO WS-USER-NAME              
             DISPLAY '2065 WS-USER-NAME: ' WS-USER-NAME                 
           ELSE                                                         
             DISPLAY '2065 SO-ENEMY-IS-ACTIVE NO ACTION   '             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2100-SEND-THE-MAP                            
      ******************************************************************
       2100-SEND-THE-MAP.                                               
      * TEST                                                            
           DISPLAY '--------------2100 SEND THE MAP '                   
           DISPLAY 'POLEUO(1) ' POLEUO(1)                               
           DISPLAY 'POLEUO(2) ' POLEUO(2)                               
           DISPLAY 'POLEUO(3) ' POLEUO(3)                               
           DISPLAY 'POLEUO(4) ' POLEUO(4)                               
           DISPLAY 'POLEUO(5) ' POLEUO(5)                               
           DISPLAY 'POLEUO(6) ' POLEUO(6)                               
           DISPLAY 'POLEUO(7) ' POLEUO(7)                               
           DISPLAY 'POLEUO(8) ' POLEUO(8)                               
           DISPLAY 'POLEUO(9) ' POLEUO(9)                               
           DISPLAY 'POLEUO(10) ' POLEUO(10)                             
      */ TEST                                                           
           EXEC CICS                                                    
             SEND MAP('MP0234') MAPSET('MP0234')                        
             FROM(MP0234O)                                              
             CURSOR                                                     
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2101-SEND-THE-CHOICE-MAP                       
      ******************************************************************
       2101-SEND-THE-CHOICE-MAP.                                        
           DISPLAY '2101 SEND THE CHOICE MAP PERFORMED '                
           EXEC CICS                                                    
             SEND MAP('MP0235') MAPSET('MP0235')                        
             FROM(MP0235O)                                              
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2102-GET-USER-CHOICE                         
      ******************************************************************
       2102-GET-USER-CHOICE.                                            
           DISPLAY '2102-GET-USER-CHOICE PERFRMED'                      
           INITIALIZE  MP0235I     
           DISPLAY '2101 MP0235I : ' MP0235I                            
           EXEC CICS                                                    
             RECEIVE MAP('MP0235') MAPSET('MP0235')                     
             INTO(MP0235I)                                              
             NOHANDLE                                                   
           END-EXEC                                                     
           DISPLAY '2102 RECEIVE COMMAND EXECUTED '                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                   2103-PROCESS-USER-CHOICE                      
      * WS-OPTION-1 WILL STORE SYMBOL THAT USER PLACED NEXT             
      * TO THE "SINGLE PLAYER" MODE                                     
      *                                                                 
      * WS-OPTION-2- WILLS TORE SYMBOL THAT USER PLACED NEXT            
      * TO THE MULTIPLAYER MODE                                         
      *                                                                 
      *                                                                 
      *  IF USER PLACED 'X' NEXT TO THE ONE OPTION THIS OPTION WILL     
      * BE PERFORMED                                                    
      * IF HE PLACED 'X' NEXT TO BOTH OPTIONS OR SYMBOL PROVIDED        
      * BY THE USER IS INVALID USER WILL GET PROPER NOTIFICATION        
      ******************************************************************
       2103-PROCESS-USER-CHOICE.                                        
           DISPLAY '2103-VALIDATE-USER-CHOICE   PERFORMED'              
                                                                        
                                                                        
           MOVE CHOIC1I TO SW-SIGLEPLAYER-FLAG                          
           MOVE CHOIC2I TO SW-MULTIPLAYER-FLAG                          
                                                                        
           EVALUATE TRUE                                                
           WHEN  SO-MULTIPLAYER-CHOSEN AND  SO-SINGLEPLAYER-CHOSEN      
      * IF USER PLACED 'X' NEXT TO THE BOTH OF MODES                    
              DISPLAY '2103 EVALAUTE1'                                  
              MOVE 'YOU CANT PLAY IN TWO MODES' TO MSG2O                
              DISPLAY 'MSG2O: ' MSG2O    
                                                                        
              PERFORM 2101-SEND-THE-CHOICE-MAP                          
           WHEN SO-SINGLEPLAYER-CHOSEN AND SO-MULTIPLAYER-EMPTY         
      * IF USER PLACED 'X' NEXT TO SINGPLE PLAYER AND NOHTING           
      * NEXT TO MULTIPLYAER                                             
              PERFORM 2251-USER-CHOSE-SINGLEPLAYER                      
           WHEN  SO-MULTIPLAYER-CHOSEN AND   SO-SINGLEPLAYER-EMPTY      
      * IF USER PLACED 'X' NEXT TO MULTIPLYAER AND NOHTING              
      * NEXT TO SINGLE PLAYER                                           
                                                                        
              PERFORM 2250-USER-CHOOSE-MULTIPLAYER                      
           WHEN OTHER                                                   
      * IF NO ABOVE OPTION WAS TRUE                                     
              DISPLAY '2103 EVALAUTE OTHER'                             
              MOVE 'PLEASE PROVIDE VALID INPUT' TO MSG2O                
              DISPLAY 'MSG2O: ' MSG2O                                   
              PERFORM 2101-SEND-THE-CHOICE-MAP                          
           END-EVALUATE                                                 
           .                                                            
                                                                        
      ******************************************************************
      *                    2105-PROTECT-USER-FIELDS                     
      * AFTER USER PROVIDED ALL OF HIS SHIPS PROGRAM WILL               
      * PROTECT FIELDS WHERE HE PROVIDED THIS DATA ( LEFT SIDE OF       
      * THE SCREEN)                                                     
      *                                                                 
      * TO PROTECT =  TO MAKE IT IMPOSSIBLE TO EDIT BY THE USER         
      *                                                                 
      ******************************************************************
       2105-PROTECT-USER-FIELDS.                                        
           DISPLAY '2105 PROTECT USER FIELDS'                           
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
                                                                        
                MOVE DFHBMPRO TO POLEUA(WS-ITER5)                       
                MOVE LOW-VALUES TO POLEKA(WS-ITER5)                     
                DISPLAY 'POLEUA( ' WS-ITER5 ' ) ' POLEUA(WS-ITER5)      
                DISPLAY 'POLEKA( ' WS-ITER5 ' ) ' POLEKA(WS-ITER5)      
           END-PERFORM                                                  
      *                                                                 
      * THIS COMMAND WILL POINT A CURSOR ON THE FIRST LINE OF THE       
      * COMPUTER BOARD                                                  
           DISPLAY '2105 WAS PERFORMED '                                
           MOVE -1 TO POLEKL(1)                                         
           DISPLAY 'POLEKL(1) VALUE  :  ' POLEKL(1)                     
           .                                                            
      ******************************************************************
      *                   2107-SAVE-COMPUTER-BOARD                      
      ******************************************************************
       2107-SAVE-COMPUTER-BOARD.                                        
           DISPLAY '  2107-SAVE-COMPUTER-BOARD PERFORMED'               
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
            MOVE WS-SCREEN-LINE(WS-ITER5) TO                            
                                        WS-COMPUTER-BOARD-LINE(WS-ITER5)
            DISPLAY ' WS-SCREEN-LINE ' WS-SCREEN-LINE(WS-ITER5)         
            DISPLAY ' WS-COMPUTER-BOARD-LINE '                          
                                     WS-COMPUTER-BOARD-LINE(WS-ITER5)   
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2108-SAVE-USER-BOARD                           
      * PROGRAM WILL MOVE ALL 'S' TO 'X' IN ORDER TO GET ALL THE        
      * VALIDTION RIGHT                                                 
      * AND AFTER THAT WE WILL REPLACING ALL 'X' SYMBOLS WITH 'S'       
      * ( USER WILL GET THE SAME DATA HE PROVIDED WHIEL PLACING         
      * THE SHIPS)                                                      
      ******************************************************************
       2108-SAVE-USER-BOARD.                                            
           DISPLAY '2108-SAVE-USER-BOARD PERFORMED '                    
            DISPLAY '2108 USER BOARD: '                                 
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
            MOVE SPACE TO WS-USER-BOARD-LINE(WS-ITER5)                  
            INSPECT WS-SCREEN-LINE(WS-ITER5) REPLACING ALL 'X' BY 'S'   
            MOVE WS-SCREEN-LINE(WS-ITER5) TO                            
                                        WS-USER-BOARD-LINE(WS-ITER5)    
            MOVE WS-SCREEN-LINE(WS-ITER5) TO POLEUO(WS-ITER5)           
            DISPLAY '2108' WS-USER-BOARD-LINE(WS-ITER5)                 
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                    2109-GET-VALUE-WHO-STARTS                    
      * PARAGRAPH WILL GET VALUE  BETWEEN  1 AND 100                    
      * IF THIS VALUE  WILL BE LESS THAN 50 THEN THIS WILL BE           
      * USER TURN IF THIS WILL BE GRATER THEN IT WILL BE COMPUTER'S     
      * TURN                                                            
      ******************************************************************
       2109-GET-VALUE-WHO-STARTS.                                       
           DISPLAY '2109-GET-VALUE-WHO-STARTS PERFORMED'                
           PERFORM 2007-GET-FIRST-SEED                                  
           DISPLAY '2109 RANDOM VALUE: ' WS-RANDOM-VALUE                
                                                                        
           IF WS-RANDOM-VALUE < 50 THEN                                 
             DISPLAY '2109 SO-USER-STARTS '                             
             SET SO-USER-STARTS TO TRUE                                 
             DISPLAY 'SW-WHO-WILL-GET-FIRST-TURN '                      
                  SW-WHO-WILL-GET-FIRST-TURN                            
           ELSE                                                         
             DISPLAY '2109 SO-COMPUTER-STARTS '                         
             SET SO-COMPUTER-STARTS TO TRUE                             
             DISPLAY 'SW-WHO-WILL-GET-FIRST-TURN '                      
                  SW-WHO-WILL-GET-FIRST-TURN                            
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                     2110-DATA-COMMAREA-TO-SCREEN                
      ******************************************************************
       2110-DATA-COMMAREA-TO-SCREEN.                                    
           DISPLAY '2110 WAS PERFORMED'                                 
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10  
                                                                        
             MOVE SPACE TO POLEUO(WS-ITER5)                             
             MOVE WS-USER-BOARD-LINE(WS-ITER5) TO POLEUO(WS-ITER5)      
             DISPLAY '2110 WS-BOARD-LINE: '                             
                             WS-USER-BOARD-LINE(WS-ITER5)               
             DISPLAY '2110 POLEUO : ' POLEUO(WS-ITER5)                  
                                                                        
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     2112-COMPUTER-SHOOTS                        
      *                                                                 
      * WE WILL GET RANDOM VALUE BETWWEN 1 AND 100 AND WE WILL CHECK    
      * IF COMPUTER CAN SHOOT THERE ( IF HE DIDN'T SHOOT THERE EIRLIER) 
      *                                                                 
      * IF NOT WE WILL SHOOT THERE                                      
      ******************************************************************
       2112-COMPUTER-SHOOTS.                                            
           DISPLAY '2112 PERFORMED '                                    
           PERFORM 2020-USERS-BOARD-TO-COMMON                           
      * LOOP WILL BE FORCIBLE BREAKED IF LOOP WILL GO MORE THATN        
      * 100 TIMES                                                       
           MOVE 0                        TO WS-ITERX                    
           SET SO-COMPUTER-HIT-SOMETHING TO TRUE                        
           SET SO-GAME-CAN-BE-CONTINUED  TO TRUE                        
           SET SO-FIND-OTHER-POSITION    TO TRUE                        
           PERFORM                                                      
             UNTIL (WS-ITERX > 100       OR                             
                   SO-GAME-SHOULD-END    OR                             
                   SO-COMPUTER-MISSED )   OR                            
                  ( SO-COMPUTER-MISSED AND SO-VALID-POSITION-WAS-FOUND )
            DISPLAY 'W PETLI 2112'                                      
           ADD 1 TO WS-ITERX                                            
      * THIS EVALUATE STATEMENT WILL DETERMINE WHAT TYPE OF             
      * SHOOT COMPUTER SHOULD DO                                        
      * THE FIRST SHOT WILL ALWAYS BE A RANDOM ONE         
      *                                                                 
      * BUT AFTER THAT PROGRAM WILL TRY TO BEHAVE LIKE A HUMAN          
      * AND WILL SHOOTING IN THE PLACES NEXT TO THE FIELDS WHERE        
      * HE HIT SOMETHING                                                
      *                                                                 
      *                                                                 
      * THERE ARE 5 TYPES OF SHOT                                       
      * 1. A RANDOM ONE                                                 
      * 2. RIGHT SHOT                                                   
      * 3. LEFT SHOT                                                    
      * 4. TOP SHOT                                                     
      * 5. BOTTOM SHOT                                                  
      *                                                                 
      * DIRECTIONAL SHOTS WILL BE MADE TO THE FIELD THAT IS CLOSEST TO  
      * THE GIVEN DIRECTION                                             
      *                                                                 
      * FOR EXAMPLE WHEN WE ARE TRYING TO DO A RIGHT SHOT               
      * AND OUR SHIP LOOK LIKE THIS XXSSX AND LAST SHOT WAS PLACED      
      * AT THE FIRST 'X' ON THIS REPRESENTATION THEN PROGRAM            
      * SHOULD FIND FIRST NOT 'X' SYMBOL AND PLACE ITS SHOT THERE       
                                                                        
      * THIS PARAGRAPH WILL VALIDATE IF ROW AND COLUMN NUMBER           
      * OF THE LAST HIT FIELD ARE VALID OR NOT                          
      *                                                                 
             PERFORM 2164-VALIDATE-LAST-HIT-POS                         
      * THIS PARAGRAPH WILL GET POSITION OF THE SHIP THAT               
      * IS AT THE SPECIFIC DIRECTION (RIGHT SIDE, LEFT SIDE, UPPER      
      * SIDE OR LOWER SIDE), PARAGRAPH CAN ALSO RANDOMLY CHOOSE A       
      * FIELD ON THE MAP                                                
      *                                                                 
      * LATER PROGRAM WILL USE POSITIONS WE GET HERE TO VALIDATE        
      * IF COMPUTER CAN FIRE THERE                                      
                                                                        
             PERFORM 2309-GET-POSITION-ENEMY-SHIP                       
                                                                        
      * IF STATEMENT BELOW WILL BE PERFORMED IF PREVIOUS PARAGRAH       
      * GOT INFO ABOUT THE FIELD WE ARE GONNA SHOT AT AND IT            
      * SEEMS TO BE VALID                                               
                                                                        
             IF SO-CONTINUE-WITH-POSITION THEN                          
      * NOW WE GOT WS-ROW-POSITION AND WS-COLUMN-POSITION VARIABLES     
      * WE WILL CHECK IF WE CAN SHOOT IN THIS POSITION                  
               SET SO-FIND-OTHER-POSITION TO TRUE                       
                                                                        
               PERFORM 2113-SHOOT-THERE-IF-POSSIBLE                     
               PERFORM 2166-DECIDE-WHERE-TO-SHOT-NEXT                   
             ELSE                                                       
               SET SO-FIND-OTHER-POSITION TO TRUE                       
             END-IF                                                     
      * IF COMPUTER MISSED OR HIT SOMETHING THEN WE WILL                
      * DISPLAY THAT SHOT ON THE SCREEN                                 
      * BUT IN OTHER CASE WE WILL PERFORM THAT LOOP ONCE AGAIN          
             PERFORM 2308-SEND-MAP-OF-COMPUTER-HIT                      
           END-PERFORM                                                  
                                                                        
           IF WS-ITERX >= CT-MAXIMAL-NUMBER-OF-SHOTS THEN               
              DISPLAY 'IT WAS IMPOSSIBLE FOR COMPUTER TO SHOOT'         
              PERFORM 3001-ERROR-EXIT                                   
           END-IF                                                       
           PERFORM 2125-CHECK-THE-TURN                                  
           PERFORM 2121-COMMON-TO-USER-BOARD                            
           .                                                            
      ******************************************************************
      *                    2113-SHOOT-THERE-IF-POSSIBLE                 
      * PARAGRAPH HAS THE POSITION OF THE FIELD WE ARE TRYING TO        
      * SHOT AT                                                         
      * HERE WE WILL VALIDATE IF WE CAN SHOOT THERE                     
      ******************************************************************
       2113-SHOOT-THERE-IF-POSSIBLE.                                    
           MOVE WS-PROGRAM-ARRAY(WS-ROW-POSITION)(WS-COLUMN-POSITION:1) 
              TO SW-WHAT-TYPE-OF-FIELD                                  
           DISPLAY '2113 PERFORMED '   
           IF    SO-HIT-FIELD OR                                        
                 SO-MISSED-FIELD OR                                     
                 SO-DESTROYED-SHIP-FIELD THEN                           
                                                                        
               DISPLAY '2113  TUTAJ JUZ BYL STRZAL  '                   
               SET SO-COMPUTER-HIT-TAKEN-FIELD TO TRUE                  
               SET SO-FIND-OTHER-POSITION      TO TRUE                  
           ELSE                                                         
               DISPLAY '2113 TUTAJ MOZNA STRZELAC '                     
               PERFORM 2310-FIRE-AT-THAT-POSITION                       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2114-CHECK-IF-DISTROYED                       
      * WS-PROGRAM-ARRAY IS THE ARRAY THAT WILL STORE USER BOARD        
      * OR COMPUTER BOARD DEPENDING ON THE SITUATION                    
      * THIS ARRAY IS USED IN ORDER TO USE THIS PARAGRAPH FOR BOTH      
      * SCENARIOS                                                       
      *                                                                 
      * PARAGRAPH ROLE IS TO DETERMINE IF SHIP WAS COMPLETLY DESTROEYED 
      * OR WAS JUST HIT                                                 
      *                                                                 
      * PROGRAM WILL CHECK WHAT IS THE ORIENTATION OF THE SHIP          
      * IF SHIP IS IN THE GOOD ORIENTATION THEN                         
      * PROGRAM WILL MOVE CURRENT BOARD ( THE BOARD WE ARE CHECKING     
      * RIGHT NOW (USER'S OR COMPUTER'S) ) TO TEMP ARRAY                
      *                                                                 
      * THEN PROGRAM WILL MARK ALL FIELDS OF THE SHIP ON THIS TEMP      
      * ARRAY AS 'Z' -> HIT AND DISTROYED, AND IF PRARAGRAPH WILL       
      * CONFIRMD THAT THIS SHIP WAS DISTORYED THIS ARRAY WILL           
      * BE SAVED AS A MAIN ONE                                          
      *                                                                 
      ******************************************************************
       2114-CHECK-IF-DISTROYED.                                         
      * PARAGRAPH WILL CHECK IN WHAT ORIENTATION THIS SHIP IS           
           DISPLAY '2114 PERFORMED '         
           MOVE WS-ROW-POSITION    TO WS-ROW-VALIDATION                
           MOVE WS-COLUMN-POSITION TO WS-COLUMN-VALIDATION             
           PERFORM 2131-VERTICAL-OR-HORIZONTAL                         
                                                                       
           IF SO-2131-ORIENTATION-VALID THEN                           
              DISPLAY '2114  SO-2131-ORIENTATION-VALID '               
              DISPLAY '2114 2131 WAS VALID '                           
              PERFORM 2134-MOVE-BOARD-TO-TEMP-ARRAY                    
              EVALUATE TRUE                                            
              WHEN SO-SHIP-IS-VERTICAL                                 
                DISPLAY '2114 SO SHIP-IS-VERTICAL '                    
                PERFORM 2132-IF-VERTICAL-DESTROYED                     
                                                                       
              WHEN SO-SHIP-IS-HORIZONTAL                               
                DISPLAY '2114 SO SHIP-IS-HORIZONTAL '                  
                PERFORM 2133-IF-HORIZONTAL-DISTROYED                   
              WHEN OTHER                                               
                DISPLAY '2114 ORHTER (EVALUATE) '                      
                MOVE 'ERROR IN 2114  (OTHER) ' TO MSGO                 
                SET SO-SERIOUS-GAME-ERROR TO TRUE                      
              END-EVALUATE                                             
      * PARAGRAPH WILL MOVE TEMPORARY ARRAY DATA TO MAIN               
      * ARRAY DATA                                                     
      *                                                                
      * WHILE CHECKING IF SHIP WAS DISTROED PARAGRAPHS WERE MARKING    
      * SHIP AS 'Z' , AND NOW WE KNOW THAT THIS SHIP WAS DISTORYED     
      * SO WE WILL SAVE THIS TEMPORARY ARRAY AS THE MAIN ONE           
              IF SO-DESTROYED-SHIP THEN                                
      * THIS IF STATEMENT WILL BE PERFORMED IF SHIP WAS COMPLETLY      
      * DISTORYED                                                      
      * THERE PROGRAM WILL MARK FIELD WHERE USER PLACED HIS SHOT       
      * AS 'Z' -> DISTROYED AND THEN TEMPORARY ARRAY WILL BE SAVED     
      * AS THE MAIN ONE                                                
                DISPLAY '2114 OSTATNI IF SHIP DISTROYED '              
                PERFORM 2136-MOVE-TEMP-ARRAY-TO-MAIN                   
              END-IF               
           ELSE                                                        
              DISPLAY '2114  SO-2131-ORIENTATION-INVALID '             
              MOVE 'ERROR IN 2131 ' TO MSGO                            
              SET SO-SERIOUS-GAME-ERROR TO TRUE                        
           END-IF                                                      
           .                                                           
      *****************************************************************
      *                 2115-MOVE-COMMAREA-TO-SCREEN                   
      * PARAGRAPH IS USED TO SEND BOARDS TO THE USER                   
      *                                                                
      * COMPUTER'S BOARD IS SEND DIFFERENTLY                           
      * (WE CANT TRANSFER LETTER 'S'  BECAUSE 'S' REPRESENTS           
      * ENEMY SHIP, AND WE DONT WANT OUR USER TO SEE ENEMY SHIPS)      
      *****************************************************************
       2115-MOVE-COMMAREA-TO-SCREEN.                                   
           DISPLAY '2115 COMMAREA TO SCREEN '                          
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10    
                MOVE WS-USER-BOARD-LINE(WS-ITER5) TO POLEUO(WS-ITER5)  
                DISPLAY 'POLEUO(  ' WS-ITER5 ' ) '                     
                                  POLEUO(WS-ITER5)                     
           END-PERFORM                                                 
      * THIS PARAGRAPH WILL MOVE ENEMY (COMPUTER BOARD) TO THE USER    
      * SCREEN                                                         
      * BUT WE CANNOT SHOW TO THE USER WHERE COMPUTER HAS HIS          
      * SHIPS  - SO WE WONT SHOW HIM 'S' SYBMOL                        
      * ALL THE REST WILL BE SENT TO HIM                               
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10    
                    PERFORM VARYING WS-ITER6 FROM 1 BY 1 UNTIL         
                                                   WS-ITER6 > 10       
                           IF WS-COMPUTER-BOARD-LINE(WS-ITER5)(        
                             WS-ITER6:1) = 'S' THEN                    
                             DISPLAY '2115 IN IF STATEMENT '           
                             DISPLAY 'WS-COMPUTER-BOARD-LINE: '        
                WS-COMPUTER-BOARD-LINE(WS-ITER5)(WS-ITER6:1)           
                             MOVE ' ' TO POLEKO(WS-ITER5)(WS-ITER6:1)  
                             DISPLAY '2115 POLEKO: '
                                       POLEKO(WS-ITER5)(WS-ITER6:1)     
                           ELSE                                         
                             MOVE WS-COMPUTER-BOARD-LINE(WS-ITER5)(     
                                 WS-ITER6:1)                            
                              TO POLEKO(WS-ITER5)(WS-ITER6:1)           
                              DISPLAY '2115 ELSE POLEKO: '              
                                       POLEKO(WS-ITER5)(WS-ITER6:1)     
                           END-IF                                       
                    END-PERFORM                                         
           END-PERFORM                                                  
      * TEST                                                            
           DISPLAY '2115 POLEUO : '                                     
           DISPLAY  POLEUO(1)                                           
           DISPLAY  POLEUO(2)                                           
           DISPLAY  POLEUO(3)                                           
           DISPLAY  POLEUO(4)                                           
           DISPLAY  POLEUO(5)                                           
           DISPLAY  POLEUO(6)                                           
           DISPLAY  POLEUO(7)                                           
           DISPLAY  POLEUO(8)                                           
           DISPLAY  POLEUO(9)                                           
           DISPLAY  POLEUO(10)                                          
           DISPLAY '2115 POLEKO : '                                     
           DISPLAY  POLEKO(1)                                           
           DISPLAY  POLEKO(2)                                           
           DISPLAY  POLEKO(3)                                           
           DISPLAY  POLEKO(4)                                           
           DISPLAY  POLEKO(5)                                           
           DISPLAY  POLEKO(6)                                           
           DISPLAY  POLEKO(7)                                           
           DISPLAY  POLEKO(8)                                           
           DISPLAY  POLEKO(9)                                           
           DISPLAY  POLEKO(10)                                          
      */ TEST                                                           
           .                                                            
      ******************************************************************
      *                      2116-USER-SHOOTS                           
      * AT THE BEGGINING USER HAS PLACE HIS CURSOR ABOVE                
      * BOARD OF THE ENEMY AND PRESS ENTER                              
      * NOW WE WILL HAVE TO GET POSITION OF HIS CURSOR                  
      ******************************************************************
       2116-USER-SHOOTS.                                                
           DISPLAY '2116-USER-SHOOTS PERFORMEED '                       
                                                                        
           PERFORM 2117-GET-CUROSR-POSITION                             
           IF SO-SHOOT-POSITION-IS-VALID  THEN                          
             DISPLAY '2116 POSITION VALID '                             
             SET SO-FIND-OTHER-POSITION TO TRUE                         
                                                                        
                                                                        
             PERFORM 2113-SHOOT-THERE-IF-POSSIBLE                       
             PERFORM 2125-CHECK-THE-TURN                                
                                                                        
             PERFORM 2119-COMMON-TO-ENEMY-BOARD                         
             IF SO-MODE-MULTIPLAYER THEN                                
                DISPLAY '2116 POSITION VALID AND MODE MULTIPLAYER '     
                PERFORM 7015-SAVE-THE-BOARD-TO-DB2                      
             END-IF                                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2117-GET-CUROSR-POSITION                      
      * LOOP IN THIS PARAGRAPH WILL GO THROUGH ALL POSSIBLE CURSOR      
      * POSISTIONS AND IF THIS CURSOR POSISTION WILL BE EQUAL TO        
      * POSITION CHOSEN BY THE USER THEN PARAGRAPH WILL BROKE THE LOOP  
      * IF THE LOOP WONT BE BROKEN IT MEANS THAT USER PLACED            
      * HIS CURSOR AT THE INVALID POSITION                              
      * ( NOT A ENEMY BOARD)                                            
      *                                                                 
      *                                                                 
      *                                                                 
      * FIRST POSITION OF THE ENEMY BOARD  IS 360 
      * LAST POSITION OF THE ENEMY BOARD IS 1080                        
      *                                                                 
      * EVERY LINE OF ENEMY BOARD IS 10 CHARACTER LONG                  
      * AND EACH LINE (WIDTH) OF THE SCREEN IS 80 CHARACTERS LONG       
      *                                                                 
      * SO FIRST POSITION OF THE BOARD ARE LIKE THIS:                   
      * 360 361 362 363 364 365 366 367 368 369                         
      * 440 441 442 443 444 445 446 447 448 449 ETC.                    
      *                                                                 
      * AFTER USER WILL PRESS ENTER WE WILL GET NUMBER ( POSITION OF    
      * CURSOR ON THE SCREEN)                                           
      * THE LOOP BELOW WILL GO IN A WAY THAT WILL GO THROUGH ALL        
      * POSSIGLE POSITIONS ON THE ENEMY BOARD (AS PRESENTED ABOVE)      
      * IF WE WILL GET A MATCH THEN IT MEANS THAT THIS IS THE           
      * POSITOIN ON THE USER BOARD                                      
      *                                                                 
      * IF WE WILL NOT GET A MATCH IT MEANS THAT USER PLACED CURSOR     
      * SOMEWHERE ELSE ON THE SCREEN NOT ON THE BOARD                   
      ******************************************************************
       2117-GET-CUROSR-POSITION.                                        
           DISPLAY '2117-GET-CUROSR-POSITION PERFORMED '                
           SET SO-SHOOT-POSITION-IS-VALID     TO TRUE                   
           DISPLAY '2116 EIBCPOSN :' EIBCPOSN                           
                                                                        
           MOVE 0 TO WS-FIELD-COUNTER                                   
           SET  SO-DONT-BREAK-OUT-OF-LOOP     TO TRUE                   
           SET  SO-USER-SHOT-POSITION-INVALID TO TRUE                   
                                                                        
           PERFORM VARYING WS-ITER6 FROM CT-FIRST-FIELD-ON-ENEMY-BOARD  
                                    BY CT-WIDTH-OF-THE-SCREEN           
                                    UNTIL WS-ITER6 >                    
                                         CT-LAST-FIELD-ON-ENEMY-BOARD   
                 OR SO-BREAK-OUT-OF-LOOP                                
                                                                        
      * WS-MAX-ROW WILL STORE A VARIABLE THAT WILL INDICATE MAXIMAL     
      * POSITION ON THIS LINE OF THE SCREEN ( ANY LARGER VALUE WILL
      * BE OUTSIDE OF THE ENEMY BOARD)                                  
                                                                        
               COMPUTE WS-MAX-ROW = WS-ITER6 +                          
                       CT-MAXIMAL-WIDTH-OF-BOARD   - 1                  
                                                                        
               PERFORM VARYING WS-ITER7 FROM WS-ITER6 BY 1 UNTIL        
                         WS-ITER7 > WS-MAX-ROW OR SO-BREAK-OUT-OF-LOOP  
                                                                        
                   ADD 1 TO WS-FIELD-COUNTER                            
                   DISPLAY 'WS-FIELD-COUNTER: ' WS-FIELD-COUNTER        
                   DISPLAY 'EIBCPOSN: '         EIBCPOSN                
                   IF WS-ITER7  = EIBCPOSN THEN                         
                      SET SO-BREAK-OUT-OF-LOOP TO TRUE                  
                   END-IF                                               
                                                                        
               END-PERFORM                                              
           END-PERFORM                                                  
      * IF THIS LOOP DIDNT BRAEK IT MEANS THAT USER PLACED A CURSOR     
      * NOT ON THE ENEMY'S BOARD                                        
           IF  SO-DONT-BREAK-OUT-OF-LOOP THEN                           
             DISPLAY '2117  SO-DONT-BREAK-OUT-OF-LOOP TRUE'             
             SET SO-USER-SHOT-POSITION-INVALID TO TRUE                  
             DISPLAY 'SO-DONT-BRAK-OUT-OF-LOOP '                        
             SET SO-SHOOT-POSITION-IS-INVALID  TO TRUE                  
             MOVE 'PLACE YOUR SHOOT IN DIFFERENT POSITION' TO MSGO      
             DISPLAY '2117 MSGO: ' MSGO                                 
             DISPLAY 'SW-IF-USER-SHOT-IS-VALID  '                       
                      SW-IF-USER-SHOT-IS-VALID                          
             DISPLAY 'SW-IF-SHOOT-WAS-VALID  '                          
                      SW-IF-SHOOT-WAS-VALID                             
             PERFORM 2100-SEND-THE-MAP                                  
           ELSE                                                         
             DISPLAY '2117  SO-DONT-BREAK-OUT-OF-LOOP FALSE '           
             SET SO-USER-SHOT-POSITION-VALID   TO TRUE                  
             MOVE EIBCPOSN                     TO WS-LAST-EIBCPOSN      
             DISPLAY 'WS-LAST-EIBCPOSN: ' WS-LAST-EIBCPOSN              
             SUBTRACT 1 FROM WS-FIELD-COUNTER                           
             MOVE WS-FIELD-COUNTER             TO WS-RANDOM-VALUE       
             PERFORM 2010-CALCULATE-POSITION                            
             DISPLAY '2117 SW-IF-USER-SHOT-IS-VALID  '                  
                       SW-IF-USER-SHOT-IS-VALID                         
             DISPLAY '2117 WS-FIEDL COUNTER: ' WS-FIELD-COUNTER         
             DISPLAY '2117 WS-RANDOM-VALUE: '  WS-RANDOM-VALUE          
             DISPLAY '2117 WS-LAST-EIBCPOSN" WS-LAST-EIBCPOSN '         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2118-ENEMY-BOARD-TO-COMMON                    
      * THIS PARAGRAPHW WILL MOVE COMPUTER (ENEMY) BOARD TO             
      * THE COMMON ARRAY THAT WILL WORK WITH BOTH BOARDS (              
      * USER AND COMPUTER )                                             
      *                                                                 
      * IT HAS TO BE DONE IN ORDER TO USER 2113 PARAGRAPH               
      * IN BOTH SCENARIOS (USER SHOOTS AND COMPUTER SHOOTS)             
      *                                                                 
      ******************************************************************
       2118-ENEMY-BOARD-TO-COMMON.                                      
           DISPLAY '2118-ENEMY-BOARD-TO-COMMON PERFORMED'               
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
              MOVE SPACE TO WS-PROGRAM-ARRAY(WS-ITER5)                  
              MOVE WS-COMPUTER-BOARD-LINE(WS-ITER5) TO                  
                            WS-PROGRAM-ARRAY(WS-ITER5)                  
              DISPLAY 'WS-PROGRAM-ARRAY ' WS-PROGRAM-ARRAY(WS-ITER5)    
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2119-COMMON-TO-ENEMY-BOARD                    
      ******************************************************************
       2119-COMMON-TO-ENEMY-BOARD.                                      
           DISPLAY ' 2119-COMMON-TO-ENEMY-BOARD PERFORMED'              
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
              MOVE WS-PROGRAM-ARRAY(WS-ITER5) TO     
                                WS-COMPUTER-BOARD-LINE(WS-ITER5)        
             DISPLAY '2119  WS-COMPUTER-BOARD-LINE: '                   
                        WS-COMPUTER-BOARD-LINE(WS-ITER5)                
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2020-USERS-BOARD-TO-COMMON                     
      * PARAGRAPH WILL MOVE DATA FROM USER'S BOARD ARRAY TO             
      * COMMON ARRAY (IN ODRER TO USE 2113 PARAGRAM FOR                 
      * BOTH SCRENATIOS 1. USER SHOOTS, 2 COMPUTER SHOOTS)              
      ******************************************************************
       2020-USERS-BOARD-TO-COMMON.                                      
           DISPLAY '2020-USERS-BOARD-TO-COMMON PERFORMED'               
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
              MOVE SPACE TO WS-PROGRAM-ARRAY(WS-ITER5)                  
              MOVE WS-USER-BOARD-LINE(WS-ITER5) TO                      
                            WS-PROGRAM-ARRAY(WS-ITER5)                  
              DISPLAY 'WS-PROGRAM-ARRAY: '                              
                            WS-PROGRAM-ARRAY(WS-ITER5)                  
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2121-COMMON-TO-USER-BOARD                      
      ******************************************************************
       2121-COMMON-TO-USER-BOARD.                                       
           DISPLAY '2121-COMMON-TO-USER-BOARD PERFORMED'                
              DISPLAY 'USER BOARD: '                                    
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
                                                                        
              MOVE SPACE TO WS-USER-BOARD-LINE(WS-ITER5)                
              MOVE WS-PROGRAM-ARRAY(WS-ITER5)   TO                      
                            WS-USER-BOARD-LINE(WS-ITER5)                
                                                                        
              DISPLAY '2121 WS-USE-BOARD: ' WS-USER-BOARD-LINE(WS-ITER5)
      * IN CASE THAT PROGRAM ARRAY HAS ANY LOW-VALUES AT THE FIRST      
      * POSISTION OF THE FIELD WE HAVE TO CHANGE IT TO SPACE        
      *                                                                 
      * LOW-VALUES AT THE FIRST POSITION WILL RESULT IN INVALID         
      * DISPLAY OF THE DATA                                             
                                                                        
      * W RAZIE BLEDU ODKOMENTUJ  (ALAMAKOT)                            
      *       IF WS-USER-BOARD-LINE(WS-ITER5)(1:1) = LOW-VALUES THEN    
      *         MOVE SPACE TO WS-USER-BOARD-LINE(WS-ITER5)(1:1)         
      *       END-IF                                                    
                                                                        
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2125-CHECK-THE-TURN                           
      * IF USER HIT ENEMY SHIP THEN USER GOT ANOTHER TURN               
      * IF HE MISSES THEN ENEMY (COMPUTER ) HAS THE TURN                
      ******************************************************************
       2125-CHECK-THE-TURN.                                             
           DISPLAY '2125 WAS PERFORMED '                                
           IF SO-VALID-POSITION-WAS-FOUND THEN                          
             DISPLAY 'SO-VALID-POSITION-WAS-FOUND'                      
             IF SO-IT-IS-SAME-SIDE-TURN THEN                            
                 DISPLAY '2125 SO-IT-IS-SAME-SIDE-TURN '                
                 IF SO-MODE-MULTIPLAYER THEN                            
                    DISPLAY '2125 SO-MODE-MULTIPLAYER '                 
                    MOVE 'YOUR TURN' TO MSGO                            
                    DISPLAY '2125 MSGO: ' MSGO                          
                 ELSE                                                   
                   DISPLAY '2125 SO-MODE IS NOT MULTIPLAYER '           
                   SET SO-TURN-NOT-CHANGED-TO-COMP  TO TRUE             
                   DISPLAY '2125:   SW-IF-TURN-CHANGED-TO-COMPUTER '    
                   DISPLAY 'SO IT IS SAME SIDE TURN '                   
                   EVALUATE TRUE                                        
                   WHEN SO-IT-IS-COMPUTERS-TURN                         
                      DISPLAY 'SO COMPUTER TURN '                       
                      MOVE 'COMPUTER TURN ' TO MSGO                     
                      DISPLAY '2125 IT IS COMPUTER TURN '  
                      DISPLAY '2125 MSGO: ' MSGO                       
                   WHEN  SO-IT-IS-USERS-TURN                           
                      DISPLAY 'SO USER TURN '                          
                      MOVE 'USER TURN ' TO MSGO                        
                      DISPLAY '2125 IT IS USER TURN '                  
                      DISPLAY '2125 MSGO: ' MSGO                       
                   WHEN OTHER                                          
                      MOVE '2125 OTHER EVALUATE (ERROR) ' TO MSGO      
                      PERFORM 2100-SEND-THE-MAP                        
                  END-EVALUATE                                         
                END-IF                                                 
             ELSE                                                      
                                                                       
      * IF TURN CHANGES THEN                                           
                 IF SO-MODE-MULTIPLAYER THEN                           
                   MOVE 'ENEMY TURN ' TO MSGO                          
                   PERFORM 7012-SWITCH-THE-TURN                        
                 ELSE                                                  
                   DISPLAY 'SO TURN CHANGES         '                  
                   EVALUATE TRUE                                       
                   WHEN SO-IT-IS-USERS-TURN                            
                      DISPLAY '2125 SO-IT-IS-USERS-TURN '              
                                                                       
                      SET SO-IT-IS-COMPUTERS-TURN TO TRUE              
                                                                       
                      SET SO-TURN-CHANGED-TO-COMPUTER TO TRUE          
                      MOVE 'COMPUTER TURN ' TO MSGO                    
                      DISPLAY '2125 MSGO: ' MSGO                       
                      DISPLAY '2125  SW-WHOS-TURN  : '                 
                                     SW-WHOS-TURN                      
                      DISPLAY '2125 SW-IF-TURN-CHANGED-TO-COMPUTER '   
                                  SW-IF-TURN-CHANGED-TO-COMPUTER       
                   WHEN  SO-IT-IS-COMPUTERS-TURN                       
                      DISPLAY '2125 SO-IT-IS-COMPUTERS-TURN'           
                      SET SO-TURN-NOT-CHANGED-TO-COMP  TO TRUE         
                      SET SO-IT-IS-USERS-TURN TO TRUE                  
                      MOVE 'USER TURN ' TO MSGO                         
                      DISPLAY '2125 MSGO: ' MSGO                        
                      DISPLAY '2125  SW-WHOS-TURN  : '                  
                                     SW-WHOS-TURN                       
                      DISPLAY '2125 SW-IF-TURN-CHANGED-TO-COMPUTER '    
                                  SW-IF-TURN-CHANGED-TO-COMPUTER        
                   WHEN OTHER                                           
                      SET SO-TURN-NOT-CHANGED-TO-COMP  TO TRUE          
                      MOVE '2125 OTHER EVALUATE (ERROR) ' TO MSGO       
                      PERFORM 2100-SEND-THE-MAP                         
                   END-EVALUATE                                         
                 END-IF                                                 
             END-IF                                                     
           ELSE                                                         
             SET SO-TURN-NOT-CHANGED-TO-COMP  TO TRUE                   
             MOVE 'PLEASE PLACE YOUR SHOT IN DIFFERENT PLACE ' TO MSGO  
             PERFORM 2100-SEND-THE-MAP                                  
           END-IF                                                       
           .                                                            
      *********** ******************************************************
      *                    2130-INITIALIZE-MAP                          
      ******************************************************************
       2130-INITIALIZE-MAP.                                             
           DISPLAY '2130-INITIALIZE-MAP PERFORMED '                     
                                                                        
           INITIALIZE MP0234I                                           
           INITIALIZE MP0234O                                           
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
              MOVE LOW-VALUES TO POLEUA(WS-ITER5)                       
              MOVE LOW-VALUES TO POLEKA(WS-ITER5)                       
              MOVE SPACE      TO POLEKI(WS-ITER5)                       
              MOVE SPACE      TO POLEKO(WS-ITER5)                       
              MOVE SPACE      TO POLEUI(WS-ITER5)                       
              MOVE SPACE      TO POLEUO(WS-ITER5)                       
      * TEST                                                            
              DISPLAY  ' POLEUA  '   POLEUA(WS-ITER5)    
              DISPLAY  ' POLEKA  '   POLEKA(WS-ITER5)                   
              DISPLAY  ' POLEKI  '   POLEKI(WS-ITER5)                   
              DISPLAY  ' POLEKO  '   POLEKO(WS-ITER5)                   
              DISPLAY  ' POLEUI  '   POLEUI(WS-ITER5)                   
              DISPLAY  ' POLEUO  '   POLEUO(WS-ITER5)                   
      */TEST                                                            
           END-PERFORM                                                  
                                                                        
           MOVE SPACE      TO MSGA                                      
           MOVE -1 TO POLEUL(1)                                         
      * TEST                                                            
           DISPLAY '2130 MSGA: ' MSGA                                   
           DISPLAY '2130 POLEUL(1) : ' POLEUL(1)                        
      */TEST                                                            
           .                                                            
      ******************************************************************
      *                   2131-VERTICAL-OR-HORIZONTAL                   
      * PARAGRAPH WILL BE CALLED IN ORDER TO DETERMINE IF THE SHIP      
      * THAT WAS HIT IS IN VERTICAL OR HORIZONTAL POSITION              
      *                                                                 
      * VARIABLES :                                                     
      *          WS-ROW-VALIDATION                                      
      *          WS-COLUMN-VALIDATION                                   
      * WILL STORE INFORMATION ABOUT THE POSITION OF THE USER/COMPUTER  
      * SHOT                                                            
      *                                                                 
      ******************************************************************
       2131-VERTICAL-OR-HORIZONTAL.                                     
           DISPLAY '2131-VERTICAL-OR-HORIZONTAL PERFORMED'              
           SET SO-SHIP-IS-NOT-HORIZONTAL  TO TRUE                       
           SET SO-SHIP-IS-NOT-VERTICAL    TO TRUE                       
           DISPLAY ' 2131 SW-IF-SHIP-IS-HORIZONTAL '                    
                           SW-IF-SHIP-IS-HORIZONTAL                     
           DISPLAY ' 2131 SW-IF-SHIP-IS-VERTICAL '                      
                           SW-IF-SHIP-IS-VERTICAL                       
      * FIRST WE WILL CHECK SHIP'S RIGHT SIDE                           
      * IF THERE IS 'S' OR 'X' SYMBOL IN MEANS THAT THIS SHIP           
      * ORIENTATION IS HORIZONTAL                                       
           IF WS-COLUMN-VALIDATION < CT-MAXIMAL-WIDTH-OF-BOARD  THEN    
               DISPLAY '2131 WS-COLUMN-VALIDATION < 10 '                
               MOVE WS-COLUMN-VALIDATION TO WS-COLUMN-TEMP              
               ADD 1 TO WS-COLUMN-TEMP                                  
               DISPLAY '2131 WS-COLUMN-VALIDATION: ' WS-COLUMN-TEMP     
               DISPLAY '2131 WS-COLUMN-TEMP: '   WS-COLUMN-TEMP         
                                                                        
               MOVE WS-PROGRAM-ARRAY(WS-ROW-VALIDATION)(                
                    WS-COLUMN-TEMP:1)  TO SW-WHAT-TYPE-OF-FIELD         
                                                                        
               IF SO-HIT-FIELD    OR  SO-SHIP-FIELD THEN                
                   SET SO-SHIP-IS-HORIZONTAL     TO TRUE                
                   DISPLAY '2131 SW-IF-SHIP-IS-HORIZONTAL '             
                              SW-IF-SHIP-IS-HORIZONTAL                  
               ELSE                                                     
                   SET SO-SHIP-IS-NOT-HORIZONTAL TO TRUE                
                   DISPLAY '2131 SW-IF-SHIP-IS-HORIZONTAL '             
                              SW-IF-SHIP-IS-HORIZONTAL                  
               END-IF                                                   
           END-IF                                                       
      * NOW WE WILL CHECK SHIP'S LEFT SIDE                              
      * IF THERE IS 'S' OR 'X' SYMBOL IN MEANS THAT THIS SHIP           
      * ORIENTATION IS HORIZONTAL                                       
      * IF WE ALREADY KNOW THAT SHIP ORIENTATION IS HORIZONTAL THEN     
      * WE WONT HAVE TO CHECK THAT ONCE AGAIN                           
      *                                                                 
           IF WS-COLUMN-VALIDATION > 1 AND SO-SHIP-IS-NOT-HORIZONTAL    
            THEN                                                        
               DISPLAY '2131 WS-COLUMN-VALIDATION > 1 AND   '           
                            ' SO-SHIP-IS-NOT-HORIZONTAL '               
                                                                        
               MOVE WS-COLUMN-VALIDATION TO WS-COLUMN-TEMP              
               SUBTRACT 1 FROM WS-COLUMN-TEMP                           
               DISPLAY 'WS-COLUMN-TEMP: '        WS-COLUMN-TEMP       
               DISPLAY 'WS-COLUMN-VALIDATION: '  WS-COLUMN-VALIDATION   
                                                                        
               MOVE WS-PROGRAM-ARRAY(WS-ROW-VALIDATION)(                
                     WS-COLUMN-TEMP:1)  TO SW-WHAT-TYPE-OF-FIELD        
               IF SO-HIT-FIELD  OR SO-SHIP-FIELD THEN                   
                   DISPLAY 'LEFT NEIGBHOUR IS A "S" OR "X" '            
                   SET SO-SHIP-IS-HORIZONTAL     TO TRUE                
                   DISPLAY '2131 SW-IF-SHIP-IS-HORIZONTAL '             
                              SW-IF-SHIP-IS-HORIZONTAL                  
               ELSE                                                     
                   DISPLAY 'LEFT NEIGBHOUR IS NOT A "S" OR "X" '        
                   SET SO-SHIP-IS-NOT-HORIZONTAL TO TRUE                
                   DISPLAY '2131 SW-IF-SHIP-IS-HORIZONTAL '             
                              SW-IF-SHIP-IS-HORIZONTAL                  
               END-IF                                                   
           END-IF                                                       
      * IF THIS SHIP IS NOT HORIZONTAL THEN WE WILL CHECK IF MAYBE IT IS
      * VERTICAL                                                        
           IF SO-SHIP-IS-NOT-HORIZONTAL AND WS-ROW-VALIDATION <         
              CT-MAXIMAL-HEIGHT-OF-BORAD  THEN                          
              DISPLAY '2131 SO-SHIP-IS-NOT-HORIZONTAL AND '             
                        ' WS-ROW-VALIDATION < 10 '                      
                                                                        
              MOVE WS-ROW-VALIDATION TO WS-ROW-TEMP                     
              ADD 1 TO WS-ROW-TEMP                                      
              DISPLAY 'WS-ROW-VALIDATION: ' WS-ROW-VALIDATION           
              DISPLAY 'WS-TEMP-ROW: ' WS-TEMP-ROW                       
                                                                        
              MOVE  WS-PROGRAM-ARRAY(WS-ROW-TEMP)(                      
                 WS-COLUMN-VALIDATION:1)  TO SW-WHAT-TYPE-OF-FIELD      
                                                                        
              IF SO-HIT-FIELD  OR  SO-SHIP-FIELD THEN                   
                 DISPLAY '2131  SO-HIT-FIELD  OR  SO-SHIP-FIELD '       
                 SET SO-SHIP-IS-VERTICAL TO TRUE                        
                 DISPLAY '2131 SW-IF-SHIP-IS-VERTICAL '                 
                       SW-IF-SHIP-IS-VERTICAL                           
                ELSE                                                      
                 DISPLAY '2131 NOT  SO-HIT-FIELD  OR  SO-SHIP-FIELD '   
                 SET SO-SHIP-IS-NOT-VERTICAL TO TRUE                    
                 DISPLAY '2131 SW-IF-SHIP-IS-VERTICAL '                 
                       SW-IF-SHIP-IS-VERTICAL                           
              END-IF                                                    
           END-IF                                                       
      * IF SHIP IS NOT HORIZONTAL AND WE STILL DON'T KNOW IF SHIP IS    
      * VERTICAL THEN WE WILL CHECK UPPER FIELD ( IF THIS FIELD         
      * WILL BE A VALID SHIP FIELD OR HIT FIELD THEN WE WILL            
      * KNOW THAT THIS IS VALID SHIP                                    
           IF SO-SHIP-IS-NOT-HORIZONTAL AND WS-ROW-VALIDATION > 1       
              AND SO-SHIP-IS-NOT-VERTICAL THEN                          
              DISPLAY ' SO-SHIP-IS-NOT-HORIZONTAL AND     '             
              DISPLAY 'WS-ROW-VALIDATION > 1   AND SHIP IS NOT VERTICAL'
                                                                        
              MOVE WS-ROW-VALIDATION TO WS-ROW-TEMP                     
              SUBTRACT 1 FROM  WS-ROW-TEMP                              
              DISPLAY '2131 WS-ROW-VALIDATION: ' WS-ROW-VALIDATION      
              DISPLAY '2131 WS-ROW-TEMP: '       WS-ROW-TEMP            
              MOVE WS-PROGRAM-ARRAY(WS-ROW-TEMP)(                       
                   WS-COLUMN-VALIDATION:1)  TO SW-WHAT-TYPE-OF-FIELD    
                                                                        
              IF SO-HIT-FIELD   OR SO-SHIP-FIELD  THEN                  
                 DISPLAY 'SO-HIT FIELD OR SO-SHIP-FIELD  '              
                 SET SO-SHIP-IS-VERTICAL TO TRUE                        
                 DISPLAY '2131 SW-IF-SHIP-IS-VERTICAL '                 
                       SW-IF-SHIP-IS-VERTICAL                           
              ELSE                                                      
                 DISPLAY ' NOT SO-HIT FIELD OR SO-SHIP-FIELD  '         
                 SET SO-SHIP-IS-NOT-VERTICAL TO TRUE                    
                 DISPLAY '2131 SW-IF-SHIP-IS-VERTICAL '                 
                       SW-IF-SHIP-IS-VERTICAL                           
              END-IF                                                    
           END-IF                                                       
      * AT THE END WE WILL CHECK IF PROGRAM CORRECTLY ASSIGNED VALUES   
      * TO THE FLAGS                                                    
      * IF WE STILL DIDN'T DETERMINE IF SHIP IS VERTICAL OR HORIZONTAL  
      * THEN WE WILL MARK THAT ORIENTATION IS INVALID                   
                                                                        
           IF  SO-SHIP-IS-VERTICAL OR SO-SHIP-IS-HORIZONTAL THEN        
             DISPLAY '2131 SHIP IS VERTICAL OR HORIZONTAL '             
             SET SO-2131-ORIENTATION-VALID TO TRUE                      
           ELSE                                                         
             DISPLAY '2131 SHIP IS NOT VERTICAL OR HORIZONTAL '         
             SET SO-2131-ORIENTATION-INVALID TO TRUE                    
             DISPLAY '2113 ORIENTATION INVALID '                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2132-IF-VERTICAL-DESTROYED                     
      * PARAGRAPH WILL BE CALLED IF SIHP WAS HIT AND THIS SHIP          
      * WAS VERTICAL                                                    
      * WE HAVE TO CHECK IF ALL OF THE SHIP WAS DISTROYED OR NOT        
      *                                                                 
      * SHIP IS VERTICAL SO WE WILL FIRST CHECK ALL "DOWN" SHIP'S       
      * FIELDS AND THEN ALL THE "UPPER" FIELDS                          
      *                                                                 
      * WE ALREADY HAVE POSITION WHERE USER OR COMPUTER                 
      * SHOOT SO WE WILL HAVE TO CHECK FILEDS LOWER AND UPPPER TO THAT  
      * IN ODRER TO DETERMINE IF SHIP WAS DISTROYED                     
      *                                                                 
      * WHILE CHECKING IF SHIP IS DISTROYED OR NOT                      
      * PROGRAM WILL BE PERFORMING 2135 PARAGRAPH THAT WILL             
      * MARK 'Z' ON THE SHIP'S FIELDS ON THE TEMORARY ARRAY             
      * IF ALL FIELDS O F THIE SHIP WAS HIT THEN                        
      * THIS TEMPORARY ARRAY WILL BE SAVED AS THE MAIN ONE              
      *                                                                 
      * IF NOT ALL FIELDS ARE HIT THEN NOTHING WILL BE DONE             
      * WITH THIS TEMPORARY ARRAY                                       
      ******************************************************************
       2132-IF-VERTICAL-DESTROYED.     
      * WE WILL CHECK THE UPPER FIELDS                                  
           DISPLAY '2132 WAS PERFORMED'                                 
           PERFORM 2138-GET-HEAD-OF-VERTICAL                            
           SET SO-NOT-END-OF-SHIP TO TRUE                               
           SET SO-DESTROYED-SHIP  TO TRUE                               
                                                                        
           PERFORM UNTIL SO-END-OF-SHIP OR  SO-NOT-DISTROYED-SHIP       
              IF WS-TEMP-ROW <= CT-MAXIMAL-HEIGHT-OF-BORAD THEN         
              DISPLAY 'WS-TEMP-ROW <= CT-MAXIMAL-HEIGHT-OF-BORAD '      
                 IF WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) =   
                   'S' OR 'X' THEN                                      
                    DISPLAY 'FIELD WE ARE CHECKING IS A "S" OR "X" '    
                                                                        
                     PERFORM 2135-MARK-SHIP-AS-DISTROYED                
                     IF WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) 
                        = 'S' THEN                                      
                       DISPLAY 'FIELD WE ARE CHECKING IS A "S"  '       
                          DISPLAY '---------'                           
                          DISPLAY '2132 IF SO-NOT-DISTROYED '           
                          DISPLAY '2132 ROW: '   WS-TEMP-ROW            
                          DISPLAY '2132 COLUMN: ' WS-TEMP-COLUMN        
                          DISPLAY '---------'                           
                          SET SO-NOT-DISTROYED-SHIP TO TRUE             
                     END-IF                                             
                 ELSE                                                   
                                                                        
                    DISPLAY 'FIELD WE ARE CHECKING IS NOT A "S" OR "X" '
                    SET SO-END-OF-SHIP TO TRUE                          
                    DISPLAY ' SW-IF-NOT-END-OF-SHIP '                   
                           SW-IF-NOT-END-OF-SHIP                        
                 END-IF                                                 
              ELSE                                                      
                DISPLAY 'WS-TEMP-ROW > CT-MAXIMAL-HEIGHT-OF-BORAD '     
                SET SO-END-OF-SHIP TO TRUE                              
                DISPLAY ' SW-IF-NOT-END-OF-SHIP '                       
                           SW-IF-NOT-END-OF-SHIP   
              END-IF                                                    
                                                                        
              ADD 1 TO WS-TEMP-ROW                                      
              DISPLAY 'AT END OF THE 2132 LOOP WS-TEMP-ROW: '           
                   WS-TEMP-ROW                                          
           END-PERFORM                                                  
                                                                        
      *     IF SO-DESTROYED-SHIP  THEN                                  
      *        DISPLAY '2132 SHIP DISTROYED '                           
      *     ELSE                                                        
      *        DISPLAY '2132 SHIP NOT DISTROYED '                       
      *     END-IF                                                      
      *                                                                 
           .                                                            
      ******************************************************************
      *               2133-IF-HORIZONTAL-DISTROYED                      
      * THIS PARAGRAPH WILL BE CALLED ONLY WHEN WE KNOW THAT USER       
      * OR COMPUTER HIT A SHIP AND THIS SHIP IS HORIZONTAL              
      *                                                                 
      * THIS PARAGRAPH WILL CHECK IF THERE IS ANY FIELD ON THAT         
      * SHIP THAT WAS NOT HIT                                           
      *                                                                 
      * WHILE CHECKINF IF SHIP WAS DISTROYED OR NOT                     
      * PROGRAM WILL MARK ALL FIELDS OF THE SHIP AS 'Z' - DISTROYED     
      * ON THE TEMPORARY ARRAY                                          
      *                                                                 
      * IF PARARAGRAPH WILL DETERMINE THAT SHIP WAS DISTORYED THEN      
      * THIS TEMPORARY ARRAY WILL BE SAVED AS THE MAIN ONE              
      * IN OTHER CASE THIS TEMPORARY WONT BE USED AT ALL                
      ******************************************************************
       2133-IF-HORIZONTAL-DISTROYED.                                    
           DISPLAY '2133 WAS PERFORMED'                                 
           SET     SO-DESTROYED-SHIP   TO TRUE                          
           SET     SO-NOT-END-OF-SHIP TO TRUE                           
           PERFORM 2137-GET-HEAD-OF-HORIZONTAL                          
      * TEST              
           DISPLAY '2133 SW-IF-SHIP-WAS-DESTROYED  '                    
                         SW-IF-SHIP-WAS-DESTROYED                       
           DISPLAY '2133 SW-IF-NOT-END-OF-SHIP '                        
                         SW-IF-NOT-END-OF-SHIP                          
      */TEST                                                            
      * THIS PARAGRAPH WILL GET ROW AND COLUMN OF THE BEGGINIG OF       
      * THE HORIZONTAL SHIP                                             
           PERFORM UNTIL SO-END-OF-SHIP OR  SO-NOT-DISTROYED-SHIP       
              IF WS-TEMP-COLUMN <= 10 THEN                              
                  DISPLAY ' 2133 WS-TEMP-COLUMN <= 10 '                 
                 IF WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) =   
                   'S' OR 'X' THEN                                      
                     DISPLAY ' 2133 CHECKED FIELD IS "S" OR "X" '       
                     PERFORM 2135-MARK-SHIP-AS-DISTROYED                
                     IF WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) 
                        = 'S' THEN                                      
                     DISPLAY ' 2133 CHECKED FIELD IS A "S" '            
                          DISPLAY '---------'                           
                          DISPLAY '2133 IF SO-NOT-DISTROYED '           
                          DISPLAY '2133 ROW: '   WS-TEMP-ROW            
                          DISPLAY '2133 COLUMN: ' WS-TEMP-COLUMN        
                          DISPLAY '---------'                           
                          SET SO-NOT-DISTROYED-SHIP TO TRUE             
                          DISPLAY ' SW-IF-SHIP-WAS-DESTROYED '          
                                 SW-IF-SHIP-WAS-DESTROYED               
                     END-IF                                             
                 ELSE                                                   
                    DISPLAY '2133 NEIGHOUBR IS OTHER THAN "S" OR "X"'   
                    SET SO-END-OF-SHIP TO TRUE                          
                    DISPLAY '2133  SW-IF-NOT-END-OF-SHIP  '             
                                   SW-IF-NOT-END-OF-SHIP                
                 END-IF                                                 
              ELSE                                                      
                SET SO-END-OF-SHIP TO TRUE                              
                DISPLAY '2133  SW-IF-NOT-END-OF-SHIP  '                 
                               SW-IF-NOT-END-OF-SHIP     
              END-IF                                                    
              ADD 1 TO WS-TEMP-COLUMN                                   
              DISPLAY '2133 WS-TEMP-CLUMN AT END OF LOOP : '            
                           WS-TEMP-COLUMN                               
           END-PERFORM                                                  
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
                                                                        
           .                                                            
      ******************************************************************
      *                  2134-MOVE-BOARD-TO-TEMP-ARRAY                  
      * PARAGRAPH WILL COPY ALL DATA FROM WS-PROGRA-ARRAY               
      * TO TEMPOARARY VERSION OF THAT ARRAY                             
      ******************************************************************
       2134-MOVE-BOARD-TO-TEMP-ARRAY.                                   
           DISPLAY '2134-MOVE-BOARD-TO-TEMP-ARRAY PERFORMED '           
                                                                        
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
             MOVE SPACE TO WS-PROGRAM-ARRAY-LINE-TEMP(WS-ITER5)         
             MOVE WS-PROGRAM-ARRAY-LINE(WS-ITER5) TO                    
                 WS-PROGRAM-ARRAY-LINE-TEMP(WS-ITER5)                   
             DISPLAY 'WS-PROGRAM-ARRAY: '                               
                         WS-PROGRAM-ARRAY-LINE(WS-ITER5)                
             DISPLAY 'WS-PROGRAM-ARRAY TEMP:   '                        
                         WS-PROGRAM-ARRAY-LINE-TEMP(WS-ITER5)           
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *               2135-MARK-SHIP-AS-DISTROYED                       
      *                                                                 
      * PARAGRAPH WILL BE CALLED WHILE CHECKING IF SHIP THAT WAS        
      * HIT WAS DISTROYED OR NOT                                        
      *                      
      * FOR EVERY FIELD THAT WILL BE CHECKED PARAGRAPH WILL MARK        
      * 'Z' ON THE TEMPORARY ARRAY                                      
      *                                                                 
      * IF PARAGRAPH WILL DETERMINE THAT THIS SHIP WAS COMPLETLY        
      * DISTROEYED THEN THIS TEMPORARY ARRAY WILL BE SAVED AS A MAIN ONE
      * THANKS TO THAT ALL FIELDS OF THE SHIP WILL BE MARKED AS 'Z'     
      ******************************************************************
       2135-MARK-SHIP-AS-DISTROYED.                                     
           DISPLAY '2135 WAS PERFORMED '                                
           DISPLAY ' 2135 ROW: '    WS-TEMP-ROW                         
           DISPLAY '2135 COLUMN: '  WS-TEMP-COLUMN                      
                                                                        
           MOVE CT-DESTROYED-SHIP-SYMBOL TO                             
            WS-PROGRAM-ARRAY-TEMP(WS-TEMP-ROW)(WS-TEMP-COLUMN:1)        
                                                                        
           DISPLAY '2135 MODIFIED FIELD: '                              
                        WS-PROGRAM-ARRAY-TEMP(WS-TEMP-ROW)(             
                            WS-TEMP-COLUMN:1)                           
           .                                                            
      ******************************************************************
      *                   2136-MOVE-TEMP-ARRAY-TO-MAIN                  
      ******************************************************************
       2136-MOVE-TEMP-ARRAY-TO-MAIN.                                    
           DISPLAY '2136 PERFORMED '                                    
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
             MOVE WS-PROGRAM-ARRAY-LINE-TEMP(WS-ITER5) TO               
                  WS-PROGRAM-ARRAY-LINE(WS-ITER5)                       
             DISPLAY '2136 ARRAY : '                                    
             DISPLAY '2136 LINE:  ' WS-PROGRAM-ARRAY-LINE(WS-ITER5)     
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2137-GET-HEAD-OF-HORIZONTAL                   
      * PARAGRAPH WILL BE CALLED IN ORDER TO DETERMINE WHEERE IS THE    
      * BEGGINING OF THE HORIZONTAL SHIP (HIS MOST LEFT FIELD)          
      * FOR EXAMPLE      
      * WHEN USER SHOOT 5 FIELD SHIP                                    
      *       ---X- WE HAVE POSITION WHERE IS AN 'X' BUT WE WANT TO     
      * GET POSITION OF THE FIRST '-' SYMBOL                            
      *                                                                 
      *                                                                 
      * AFTER DOING SO PROGRAM WILL BE ABLE TO SIMPLY CHECK             
      * IF SHIP WAS DISTROYED OR NOT                                    
      *                                                                 
      ******************************************************************
       2137-GET-HEAD-OF-HORIZONTAL.                                     
           SET  SO-NOT-END-OF-SHIP   TO TRUE                            
           MOVE WS-ROW-VALIDATION    TO WS-TEMP-ROW                     
           MOVE WS-COLUMN-VALIDATION TO WS-TEMP-COLUMN                  
           DISPLAY '2137 AT START VALUES '                              
           DISPLAY '2137 ROW: '      WS-ROW-VALIDATION                  
           DISPLAY '2137 COLUMN: '   WS-COLUMN-VALIDATION               
           DISPLAY '2137 TEMP ROW: '      WS-TEMP-ROW                   
           DISPLAY '2137 TEMP COLUMN: '   WS-TEMP-COLUMN                
           DISPLAY '2137 SW-IF-NOT-END-OF-SHIP  '                       
                        SW-IF-NOT-END-OF-SHIP                           
           PERFORM UNTIL SO-END-OF-SHIP                                 
                                                                        
              SUBTRACT 1 FROM WS-TEMP-COLUMN                            
              DISPLAY 'WS-TEMP-COLUMN AFTER SUBSTRACTION: '             
                                  WS-TEMP-COLUMN                        
              IF WS-TEMP-COLUMN > 0  THEN                               
                IF WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) =    
                   'S' OR 'X' THEN                                      
                                                                        
                     DISPLAY ' FIELD IS "S" OR "X" NO ACTION TAKEN '    
      *              CONTINUE                                           
                ELSE                                                    
                   DISPLAY 'FIELD IS NOT "S" OR "X" '                   
                                                                        
                   SET SO-END-OF-SHIP TO TRUE                           
                   ADD 1 TO WS-TEMP-COLUMN      
                   DISPLAY '2137 SW-IF-NOT-END-OF-SHIP  '               
                        SW-IF-NOT-END-OF-SHIP                           
                   DISPLAY 'WS-TEMP-COLUMN: ' WS-TEMP-COLUMN            
                                                                        
                END-IF                                                  
              ELSE                                                      
                SET SO-END-OF-SHIP TO TRUE                              
                ADD 1 TO WS-TEMP-COLUMN                                 
              END-IF                                                    
                                                                        
           END-PERFORM                                                  
           MOVE WS-TEMP-ROW    TO  WS-ROW-VALIDATION                    
           MOVE WS-TEMP-COLUMN TO  WS-COLUMN-VALIDATION                 
           DISPLAY 'AFTER 2137 ROW AND COLUN: '                         
           DISPLAY '2137 ROW: '    WS-ROW-VALIDATION                    
           DISPLAY '2137 COLUMN: ' WS-COLUMN-VALIDATION                 
           .                                                            
      ******************************************************************
      *                   2138-GET-HEAD-OF-VERTICAL                     
      ******************************************************************
       2138-GET-HEAD-OF-VERTICAL.                                       
           SET SO-NOT-END-OF-SHIP TO TRUE                               
           MOVE WS-ROW-VALIDATION    TO WS-TEMP-ROW                     
           MOVE WS-COLUMN-VALIDATION TO WS-TEMP-COLUMN                  
           DISPLAY '2138 AT START VALUES '                              
           DISPLAY '2138 ROW: '      WS-TEMP-ROW                        
           DISPLAY '2138 COLUMN: '   WS-TEMP-COLUMN                     
           DISPLAY '2138 SW-IF-NOT-END-OF-SHIP  '                       
                        SW-IF-NOT-END-OF-SHIP                           
           PERFORM UNTIL SO-END-OF-SHIP                                 
                                                                        
              DISPLAY '2138 IN LOOP BEFORE SUBSTRACTION: '              
                                      WS-TEMP-ROW                       
              SUBTRACT 1 FROM WS-TEMP-ROW                               
              DISPLAY '2138 IN LOOP AFTER SUBSTRACTION: '               
                                      WS-TEMP-ROW                       
              IF WS-TEMP-ROW    > 0 THEN                                
                IF WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) =    
                   'S' OR 'X' THEN                                      
      *             CONTINUE                                            
                     DISPLAY '2138 NO ACTION TAKEN '                    
                ELSE                                                    
                   SET SO-END-OF-SHIP TO TRUE                           
                   ADD 1 TO WS-TEMP-ROW                                 
                END-IF                                                  
              ELSE                                                      
                SET SO-END-OF-SHIP TO TRUE                              
                ADD 1 TO WS-TEMP-ROW                                    
              END-IF                                                    
                                                                        
           END-PERFORM                                                  
           MOVE WS-TEMP-ROW    TO       WS-ROW-VALIDATION               
           MOVE WS-TEMP-COLUMN TO       WS-COLUMN-VALIDATION            
           DISPLAY 'AFTER 2138 ROW AND COLUN: '                         
           DISPLAY '2138 ROW: '         WS-ROW-VALIDATION               
           DISPLAY '2138 COLUMN: '      WS-COLUMN-VALIDATION            
           DISPLAY '2138 TEMP ROW: '    WS-TEMP-ROW                     
           DISPLAY '2138 TEMP COLUMN: ' WS-TEMP-COLUMN                  
           .                                                            
      ******************************************************************
      *                   2139-IF-GAME-ENDED-OR-NOT                     
      * PARAGRAPH WILL CHECK IF THERE ARE ANY SHIPS (NOT DESTORYED)     
      * ON THE BOARDS                                                   
      * IF THERE IS NO 'S' SYMBOLS IT INDICATES THAT THERE IS NO        
      * SHIP ON HIS BOARD                                               
      *                                                                 
      * IF ONE OF THE BOARDS IS EMPTY THEN WE WILL DETERMINE WHO IS     
      * THE WINNER (THE WINNER IS PLAYER WITH NUMBER OF SHIPS           
      * GREATER THAN ZERO)                                              
      ******************************************************************
       2139-IF-GAME-ENDED-OR-NOT.                                       
           DISPLAY '2139 IF-GAME-ENEDED-OR-NOT PERFORMED'     
           MOVE 0 TO WS-ENEMY-BOARD-COUNTER                             
           MOVE 0 TO WS-USER-BOARD-COUNTER                              
           DISPLAY '2139 WS-ENEMY-BOARD-COUNTER: '                      
                    WS-ENEMY-BOARD-COUNTER                              
           DISPLAY '2139 WS-USER-BOARD-COUNTER: '                       
                    WS-USER-BOARD-COUNTER                               
                                                                        
           PERFORM VARYING WS-ITER1 FROM 1 BY 1 UNTIL WS-ITER1 > 10     
                                                                        
             INSPECT WS-COMPUTER-BOARD-LINE(WS-ITER1) TALLYING          
                  WS-SHIP-COUNTER FOR ALL 'S'                           
             ADD WS-SHIP-COUNTER TO WS-ENEMY-BOARD-COUNTER              
             MOVE 0 TO WS-SHIP-COUNTER                                  
                                                                        
             INSPECT WS-USER-BOARD-LINE(WS-ITER1) TALLYING              
                  WS-SHIP-COUNTER FOR ALL 'S'                           
             ADD WS-SHIP-COUNTER TO WS-USER-BOARD-COUNTER               
             MOVE 0 TO WS-SHIP-COUNTER                                  
                                                                        
             DISPLAY 'IN LOOP WS-USER-BOARD-COUNTER : '                 
                             WS-USER-BOARD-COUNTER                      
             DISPLAY 'IN LOOP WS-ENEMY-BOARD-COUNTER: '                 
                              WS-ENEMY-BOARD-COUNTER                    
           END-PERFORM                                                  
                                                                        
           DISPLAY 'WS-ENEMY-BOARD-COUNTER ' WS-ENEMY-BOARD-COUNTER     
           DISPLAY 'WS-USER-BOARD-COUNTER ' WS-USER-BOARD-COUNTER       
                                                                        
           IF WS-USER-BOARD-COUNTER > 0 AND WS-ENEMY-BOARD-COUNTER > 0  
           THEN                                                         
             DISPLAY ' BOTH PLAYERS HAD SHIPS '                         
             SET SO-GAME-CAN-BE-CONTINUED TO TRUE                       
           ELSE                                                         
             IF WS-USER-BOARD-COUNTER = 0 THEN                          
               DISPLAY '2139 USER DONT HAVE ANY SHIPS'                  
               SET SO-COMPUTER-WON TO TRUE                  
               MOVE 'YOU WERE DEFEATED ' TO MSGO                        
               PERFORM 7020-DELETE-THIS-GAME-DATA                       
             ELSE                                                       
               DISPLAY '2139 ENEMY DONT HAVE ANY SHIPS'                 
               SET SO-USER-WON TO TRUE                                  
               MOVE 'YOU WIN ' TO MSGO                                  
      * IF ONE OF THE USER WON AND OTHER IS STILL WAIT FOR ITS TURN     
      * WE WILL MODIFY THE "PLAYER WITH TURN" TO 'AAAAAA' THAT WILL     
      * INDICATE THAT ENEMY WON THE GAME AND PROGRAM SOULD NOT          
      * WAIT FOR ITS TURN                                               
               SET SO-THIS-PLAYER-WIN TO TRUE                           
               PERFORM 7012-SWITCH-THE-TURN                             
             END-IF                                                     
             SET SO-GAME-SHOULD-END       TO TRUE                       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2140-PREPARE-NEXT-SHOT                         
      * PARAGRAPH WILL BE CALLED IN TWO SCENARIOS :                     
      * 1. COMPUTER HIT SOMETHING                                       
      * 2. COMPUTER MISSED BUT HE IS TRYING TO HIT NEXT TO THE          
      *    SHIP HE HIT EARILER                                          
      *                                                                 
      * (PARAGRAPH WONT BE CALLED WHEN PROGRAM MISSED AN RANDOM SHOT)   
      *                                                                 
      * AFTER THIS PARAGRAPH WILL END WE WILL SAVE DIRECTION            
      * (TOP, BOTTOM, LEFT OR RIGHT) INTO THE COMMAREA AND NEXT         
      * TIME COMPUTER WILL HAVE TO SHOOT THEN HE WILL SHOOT IN THAT     
      * DIRECTION                                                       
      ******************************************************************
       2140-PREPARE-NEXT-SHOT.                                          
           DISPLAY '2140 PERFORMED'                                     
           DISPLAY '2140 ROW AND COLUMN: '                              
           DISPLAY 'ROW: '    WS-ROW-POSITION                           
           DISPLAY 'COLUMN: ' WS-COLUMN-POSITION                        
                                                                        
           PERFORM 2171-PREP-NEIGHBOURS-CHECK                           
                                                                        
           SET SO-HORIZONTAL-CHECK TO TRUE                              
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 2      
             DISPLAY 'IN LOOP OF 2195'                                  
             PERFORM 2205-CHECK-NEIGHBOURS                              
             SET SO-VERTICAL-CHECK TO TRUE                              
           END-PERFORM                                                  
                                                                        
      * PARAGRAPH WILL CHOOSE A SIDE RANDOMLY WHEN IN COULD NOT         
      * DETERMINE WHAT SIDE IS VALID                                    
           PERFORM 2198-CHOOSE-RANDOM-SIDE                              
           .                                                            
      ******************************************************************
      *                   2149-GET-RIGHT-SIDE-SHOT-POS                  
      * THE TASK OF THE PARAGRAPH IS TO FIND THE RIGHTMOST PART OF THE  
      * SHIP THAT HAS NOT YET BEEN HIT                                  
      ******************************************************************
       2149-GET-RIGHT-SIDE-SHOT-POS.                                    
           DISPLAY ' 2149-GET-RIGHT-SIDE-SHOT-POS PERFORMED'            
           SET SO-NOT-END-OF-SHIP                  TO TRUE              
           MOVE WS-ROW-POSITION                    TO  WS-TEMP-ROW      
           MOVE WS-COLUMN-POSITION                 TO  WS-TEMP-COLUMN   
                                                                        
           DISPLAY '2149 WS-ROW-POSITION      ' WS-ROW-POSITION         
           DISPLAY '2149 WS-COLUMN-POSITION   ' WS-COLUMN-POSITION      
           DISPLAY '2149 WS-TEMP-ROW          ' WS-TEMP-ROW             
           DISPLAY '2149 WS-TEMP-COLUMN       ' WS-TEMP-COLUMN          
           DISPLAY '2149 SW-IF-NOT-END-OF-SHIP  ' SW-IF-NOT-END-OF-SHIP 
                                                                        
           PERFORM UNTIL SO-END-OF-SHIP                                 
              ADD 1 TO WS-TEMP-COLUMN                                   
              IF WS-TEMP-COLUMN <= 10 THEN                              
               EVALUATE WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) 
                 WHEN 'X'                                               
      *             CONTINUE                                            
                    DISPLAY ' 2149  "X" NO ACTION  '                    
                 WHEN 'O'                                               
                    DISPLAY '2149 "O" '                                 
                    SET SO-RANDOM-SHOT                TO TRUE           
                    SET SO-END-OF-SHIP                TO TRUE           
                    SET SO-NOT-CONTINUE-WITH-POSITION TO TRUE           
                    DISPLAY 'SW-WHAT-COMPUTER-SHOULD-DO '               
                             SW-WHAT-COMPUTER-SHOULD-DO                 
                    DISPLAY 'SW-IF-NOT-END-OF-SHIP   '                  
                             SW-IF-NOT-END-OF-SHIP                      
                    DISPLAY 'SW-IF-CONTINUATION-ALLOWED '               
                             SW-IF-CONTINUATION-ALLOWED                 
                 WHEN OTHER                                             
                    SET  SO-END-OF-SHIP               TO TRUE           
                    SET  SO-CONTINUE-WITH-POSITION    TO TRUE           
                    MOVE WS-TEMP-ROW                 TO  WS-ROW-POSITION
                    MOVE WS-TEMP-COLUMN           TO  WS-COLUMN-POSITION
                    DISPLAY '2149 WHEN OTHER '                          
                    DISPLAY '2149 WS-TEMP-ROW '       WS-TEMP-ROW       
                    DISPLAY '2149 WS-TEMP-COLUMN '    WS-TEMP-COLUMN    
                    DISPLAY '2149 WS-ROW-POSITION  '  WS-ROW-POSITION   
                    DISPLAY '2149WS-COLUMN-POSITION ' WS-COLUMN-POSITION
                    DISPLAY 'SW-IF-NOT-END-OF-SHIP   '                  
                             SW-IF-NOT-END-OF-SHIP                      
                    DISPLAY 'SW-IF-CONTINUATION-ALLOWED '               
                             SW-IF-CONTINUATION-ALLOWED                 
               END-EVALUATE                                             
              ELSE                                                      
                DISPLAY 'WS-TEMP-COLUMN > 10 '                          
                SET SO-LEFT-SHOT                  TO TRUE               
                SET SO-END-OF-SHIP                TO TRUE               
                SET SO-NOT-CONTINUE-WITH-POSITION TO TRUE               
                    DISPLAY 'SW-WHAT-COMPUTER-SHOULD-DO '               
                             SW-WHAT-COMPUTER-SHOULD-DO                 
                    DISPLAY 'SW-IF-NOT-END-OF-SHIP   '                  
                             SW-IF-NOT-END-OF-SHIP                      
                    DISPLAY 'SW-IF-CONTINUATION-ALLOWED '               
                             SW-IF-CONTINUATION-ALLOWED                 
              END-IF                                                    
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2150-GET-LEFT-SHOT-POSITION                   
      * THE TASK OF THE PARAGRAPH IS TO FIND THE LEFTMOST PART OF THE   
      * SHIP THAT HAS NOT YET BEEN HIT                                  
      ******************************************************************
       2150-GET-LEFT-SHOT-POSITION.                                     
                                                                        
           SET SO-NOT-END-OF-SHIP                  TO TRUE              
           MOVE WS-ROW-POSITION                    TO  WS-TEMP-ROW      
           MOVE WS-COLUMN-POSITION                 TO  WS-TEMP-COLUMN   
      * TEST                                                            
           DISPLAY '  2150-GET-LEFT-SHOT-POSITION PERFORMED'            
           DISPLAY ' 2150  WS-ROW-POSITION     ' WS-ROW-POSITION        
           DISPLAY ' 2150  WS-COLUMN-POSITION  ' WS-COLUMN-POSITION     
           DISPLAY ' 2150  WS-TEMP-ROW         ' WS-TEMP-ROW            
           DISPLAY ' 2150  WS-TEMP-COLUMN      ' WS-TEMP-COLUMN         
           DISPLAY ' 2150  SW-IF-NOT-END-OF-SHIP ' SW-IF-NOT-END-OF-SHIP
      * /TEST                                                           
           PERFORM UNTIL SO-END-OF-SHIP                                 
             SUBTRACT 1 FROM WS-TEMP-COLUMN                             
              IF WS-TEMP-COLUMN >  0 THEN                               
               EVALUATE WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) 
                 WHEN 'X'                                               
      *             CONTINUE                                            
                    DISPLAY '2150 WHEN "X" NO ACTION TAKEN '            
                 WHEN 'O'                                               
                    DISPLAY 'WHEN "0" '                                 
                    SET SO-RANDOM-SHOT                TO TRUE           
                    SET SO-END-OF-SHIP                TO TRUE           
                    SET SO-NOT-CONTINUE-WITH-POSITION TO TRUE   
                    DISPLAY 'SW-WHAT-COMPUTER-SHOULD-DO '               
                             SW-WHAT-COMPUTER-SHOULD-DO                 
                    DISPLAY 'SW-IF-NOT-END-OF-SHIP  '                   
                             SW-IF-NOT-END-OF-SHIP                      
                    DISPLAY ' SW-IF-CONTINUATION-ALLOWED '              
                              SW-IF-CONTINUATION-ALLOWED                
                 WHEN OTHER                                             
                    DISPLAY 'WHEN OTHER '                               
                    SET  SO-END-OF-SHIP            TO TRUE              
                    SET  SO-CONTINUE-WITH-POSITION TO TRUE              
                    MOVE WS-TEMP-ROW               TO  WS-ROW-POSITION  
                    MOVE WS-TEMP-COLUMN           TO  WS-COLUMN-POSITION
      * TEST                                                            
                    DISPLAY 'SW-IF-NOT-END-OF-SHIP  '                   
                             SW-IF-NOT-END-OF-SHIP                      
                    DISPLAY ' SW-IF-CONTINUATION-ALLOWED '              
                              SW-IF-CONTINUATION-ALLOWED                
                    DISPLAY '2150: WS-TEMP-ROW        ' WS-TEMP-ROW     
                    DISPLAY '2150: WS-TEMP-COLUMN     ' WS-TEMP-COLUMN  
                    DISPLAY '2150: WS-ROW-POSITION    ' WS-ROW-POSITION 
                    DISPLAY '2150: WS-COLUMN-POSITION '                 
                                   WS-COLUMN-POSITION                   
      */TEST                                                            
               END-EVALUATE                                             
             ELSE                                                       
                DISPLAY 'WS-TEMP-COLUMN < = 0 '                         
                SET SO-RIGHT-SHOT                 TO TRUE               
                SET SO-END-OF-SHIP                TO TRUE               
                SET SO-NOT-CONTINUE-WITH-POSITION TO TRUE               
      * TEST                                                            
                    DISPLAY 'SW-WHAT-COMPUTER-SHOULD-DO '               
                             SW-WHAT-COMPUTER-SHOULD-DO                 
                    DISPLAY 'SW-IF-NOT-END-OF-SHIP  '                   
                             SW-IF-NOT-END-OF-SHIP                      
                    DISPLAY ' SW-IF-CONTINUATION-ALLOWED '              
                              SW-IF-CONTINUATION-ALLOWED      
      */TEST                                                            
             END-IF                                                     
           END-PERFORM                                                  
                                                                        
           .                                                            
      ******************************************************************
      *                   2151-GET-UPPER-SHOT-POSITION                  
      * THE TASK OF THE PARAGRAPH IS TO FIND THE UPPERMOST PART OF THE  
      * SHIP THAT HAS NOT YET BEEN HIT                                  
      ******************************************************************
       2151-GET-UPPER-SHOT-POSITION.                                    
           SET SO-NOT-END-OF-SHIP                  TO TRUE              
           MOVE WS-ROW-POSITION                    TO  WS-TEMP-ROW      
           MOVE WS-COLUMN-POSITION                 TO  WS-TEMP-COLUMN   
      * TEST                                                            
           DISPLAY '2151: WS-ROW-POSITION    ' WS-ROW-POSITION          
           DISPLAY '2151: WS-COLUMN-POSITION ' WS-COLUMN-POSITION       
           DISPLAY '2151: WS-TEMP-ROW        ' WS-TEMP-ROW              
           DISPLAY '2151: WS-TEMP-COLUMN     ' WS-TEMP-COLUMN           
           DISPLAY '2151: SW-IF-NOT-END-OF-SHIP '                       
                          SW-IF-NOT-END-OF-SHIP                         
      */TEST                                                            
           PERFORM UNTIL SO-END-OF-SHIP                                 
             SUBTRACT 1 FROM WS-TEMP-ROW                                
              DISPLAY 'WS-TEMP-ROW AFTER SUBSTRACTION: '                
                              WS-TEMP-ROW                               
                                                                        
              IF WS-TEMP-ROW    >  0 THEN                               
               EVALUATE WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) 
                 WHEN 'X'                                               
      *           CONTINUE                                              
                   DISPLAY 'WHEN "X" NO ACTION TAKEN '                  
                 WHEN 'O'                                               
                   DISPLAY 'WHEN "O" '                                  
                    SET SO-RANDOM-SHOT                TO TRUE           
                    SET SO-END-OF-SHIP                TO TRUE    
                    SET SO-NOT-CONTINUE-WITH-POSITION TO TRUE           
                    DISPLAY 'SW-WHAT-COMPUTER-SHOULD-DO '               
                             SW-WHAT-COMPUTER-SHOULD-DO                 
                    DISPLAY 'SW-IF-NOT-END-OF-SHIP  '                   
                             SW-IF-NOT-END-OF-SHIP                      
                    DISPLAY ' SW-IF-CONTINUATION-ALLOWED '              
                              SW-IF-CONTINUATION-ALLOWED                
                 WHEN OTHER                                             
                   DISPLAY 'WHEN OTHER '                                
                    SET  SO-END-OF-SHIP            TO TRUE              
                    SET  SO-CONTINUE-WITH-POSITION TO TRUE              
                    MOVE WS-TEMP-ROW               TO  WS-ROW-POSITION  
                    MOVE WS-TEMP-COLUMN           TO  WS-COLUMN-POSITION
      * TEST                                                            
                    DISPLAY 'SW-IF-NOT-END-OF-SHIP  '                   
                             SW-IF-NOT-END-OF-SHIP                      
                    DISPLAY ' SW-IF-CONTINUATION-ALLOWED '              
                              SW-IF-CONTINUATION-ALLOWED                
                    DISPLAY '2151: WS-TEMP-ROW        ' WS-TEMP-ROW     
                    DISPLAY '2151: WS-TEMP-COLUMN     ' WS-TEMP-COLUMN  
                    DISPLAY '2151: WS-ROW-POSITION    ' WS-ROW-POSITION 
                    DISPLAY '2151: WS-COLUMN-POSITION '                 
                                   WS-COLUMN-POSITION                   
      */TEST                                                            
               END-EVALUATE                                             
             ELSE                                                       
                DISPLAY ' WS-TEMP-ROW   <=  0 '                         
                SET SO-BOTTOM-SHOT             TO TRUE                  
                SET SO-END-OF-SHIP             TO TRUE                  
                SET SO-NOT-CONTINUE-WITH-POSITION TO TRUE               
      * TEST                                                            
                DISPLAY 'SW-WHAT-COMPUTER-SHOULD-DO '                   
                             SW-WHAT-COMPUTER-SHOULD-DO                 
                DISPLAY 'SW-IF-NOT-END-OF-SHIP  '                       
                             SW-IF-NOT-END-OF-SHIP                      
                DISPLAY ' SW-IF-CONTINUATION-ALLOWED '                  
                              SW-IF-CONTINUATION-ALLOWED                
      */TEST                                                            
             END-IF                                                     
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2152-GET-LOWER-SHOT-POSITION                  
      * THE TASK OF THE PARAGRAPH IS TO FIND THE LOWERMOST PART OF THE  
      * SHIP THAT HAS NOT YET BEEN HIT                                  
      ******************************************************************
       2152-GET-LOWER-SHOT-POSITION.                                    
           SET SO-NOT-END-OF-SHIP                  TO TRUE              
           MOVE WS-ROW-POSITION                    TO  WS-TEMP-ROW      
           MOVE WS-COLUMN-POSITION                 TO  WS-TEMP-COLUMN   
      * TEST                                                            
           DISPLAY '2151: WS-ROW-POSITION    ' WS-ROW-POSITION          
           DISPLAY '2151: WS-COLUMN-POSITION ' WS-COLUMN-POSITION       
           DISPLAY '2151: WS-TEMP-ROW        ' WS-TEMP-ROW              
           DISPLAY '2151: WS-TEMP-COLUMN     ' WS-TEMP-COLUMN           
           DISPLAY '2151: SW-IF-NOT-END-OF-SHIP '                       
                          SW-IF-NOT-END-OF-SHIP                         
      */TEST                                                            
           PERFORM UNTIL SO-END-OF-SHIP                                 
              ADD 1 TO WS-TEMP-ROW                                      
              DISPLAY '2152 START OF THE LOOP WS-TEMP-ROW: '            
              IF WS-TEMP-ROW   <= 10 THEN                               
               EVALUATE WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) 
                 WHEN 'X'                                               
                   DISPLAY 'WHEN "X" NO ACTION '                        
                 WHEN 'O'                                               
                    SET SO-RANDOM-SHOT             TO TRUE              
                    SET SO-END-OF-SHIP             TO TRUE              
                    SET SO-NOT-CONTINUE-WITH-POSITION TO TRUE           
                    DISPLAY 'SW-WHAT-COMPUTER-SHOULD-DO '               
                             SW-WHAT-COMPUTER-SHOULD-DO                 
                    DISPLAY 'SW-IF-NOT-END-OF-SHIP  '
                             SW-IF-NOT-END-OF-SHIP                      
                    DISPLAY ' SW-IF-CONTINUATION-ALLOWED '              
                              SW-IF-CONTINUATION-ALLOWED                
                 WHEN OTHER                                             
                    SET  SO-END-OF-SHIP            TO TRUE              
                    SET  SO-CONTINUE-WITH-POSITION TO TRUE              
                    MOVE WS-TEMP-ROW               TO  WS-ROW-POSITION  
                    MOVE WS-TEMP-COLUMN           TO  WS-COLUMN-POSITION
      * TEST                                                            
                    DISPLAY 'SW-IF-NOT-END-OF-SHIP  '                   
                             SW-IF-NOT-END-OF-SHIP                      
                    DISPLAY ' SW-IF-CONTINUATION-ALLOWED '              
                              SW-IF-CONTINUATION-ALLOWED                
                    DISPLAY '2151: WS-TEMP-ROW        ' WS-TEMP-ROW     
                    DISPLAY '2151: WS-TEMP-COLUMN     ' WS-TEMP-COLUMN  
                    DISPLAY '2151: WS-ROW-POSITION    ' WS-ROW-POSITION 
                    DISPLAY '2151: WS-COLUMN-POSITION '                 
                                   WS-COLUMN-POSITION                   
      */TEST                                                            
               END-EVALUATE                                             
             ELSE                                                       
                SET SO-TOP-SHOT             TO TRUE                     
                SET SO-END-OF-SHIP             TO TRUE                  
                SET SO-NOT-CONTINUE-WITH-POSITION TO TRUE               
      * TEST                                                            
                DISPLAY 'SW-WHAT-COMPUTER-SHOULD-DO '                   
                             SW-WHAT-COMPUTER-SHOULD-DO                 
                DISPLAY 'SW-IF-NOT-END-OF-SHIP  '                       
                             SW-IF-NOT-END-OF-SHIP                      
                DISPLAY ' SW-IF-CONTINUATION-ALLOWED '                  
                              SW-IF-CONTINUATION-ALLOWED                
      */TEST                                                            
             END-IF                                                     
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                 2154-INITIALIZE-USER-MAP                        
      * PARAGRAPH WILL BE CALLED WHEN USER PROVIDED INVALID             
      * REPRESENTATION OF THE SHIPS                                     
      *                                                                 
      * SHIP'S DATA WILL BE REMOVED FROM PROGRAM COMMAREA AND           
      * FROM USER SCREEN (USER WILL HAVE TO PROVIDE IT WHOLE ONCE       
      * AGAIN)                                                          
      ******************************************************************
       2154-INITIALIZE-USER-MAP.                                        
           DISPLAY '2154-INITIALIZE-USER-MAP PERFORMED'                 
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
              MOVE SPACE TO WS-USER-BOARD-LINE(WS-ITER5)                
              MOVE SPACE TO POLEUO(WS-ITER5)                            
              DISPLAY 'POLEUO ' WS-ITER5 ' ' POLEUO(WS-ITER5)           
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2161-CHECK-IF-ENDGAME                         
      ******************************************************************
       2161-CHECK-IF-ENDGAME.                                           
           DISPLAY '2161 CHECK IF ENDGAME PERFORMED '                   
           PERFORM 2139-IF-GAME-ENDED-OR-NOT                            
           IF SO-GAME-SHOULD-END THEN                                   
              DISPLAY '2161 SO GAME SHOULD END'                         
              STRING  'GAME ENDS  WINNER '                              
                      , SW-WHO-IS-THE-WINNER                            
                DELIMITED BY SIZE                                       
                INTO MSGO                                               
              END-STRING                                                
              DISPLAY 'MSGO: ' MSGO                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2162-CHECK-HORIZONTAL-FIELDS                   
      * THIS PARAGRAPH WILL CHECK EVERY POSITION OF THE SHIP            
      * (HORIZONTALLY ) EXCEPT FOR THE FIRST ONE                        
      *                                                                 
      * IT WILL CHECK IF ALL OF THOSE FIELD CAN BE A VALID PART OF      
      * THE SHIP, IF ANY OF THOSE FIELD HAS A NEIGHBOUR OR ARE INVALID  
      * SYBOL THEN THIS VALIDATION WILL BE BE STOPPED (SHIP IS INVALID) 
      *                                                                 
      * IF EVERYTHING GOES WELL THEN THIS PARAGRAPH WILL VALIDATE       
      * THAT ALL THOSE FIELDS ARE VALID AND CAN BE A SHIP               
      ******************************************************************
       2162-CHECK-HORIZONTAL-FIELDS.                                    
           DISPLAY '2162-CHECK-HORIZONTAL-FIELDS PERFORMED'             
                                                                        
           PERFORM UNTIL SO-END-OF-SHIP OR SO-INVALID-USER-SHIPS        
               DISPLAY '2162 LOOP STARTS'                               
               ADD 1 TO WS-ITER-COLUMN-TEMP                             
               DISPLAY '2162 LOOP STARTS AFTER ADDING WS-ITER COL: '    
                            WS-ITER-COLUMN-TEMP                         
                                                                        
               IF WS-SCREEN-TABLE(WS-ITER-ROW)(WS-ITER-COLUMN-TEMP:1) = 
                'S' AND WS-ITER-COLUMN-TEMP <= 10  THEN                 
                  DISPLAY '2162 IN THE IF STATEMENT '                   
                  DISPLAY                                               
                  'WS-SCREEN-TABLE(WS-ITER-ROW)(WS-ITER-COLUMN-TEMP:1) '
                  WS-SCREEN-TABLE(WS-ITER-ROW)(WS-ITER-COLUMN-TEMP:1)   
                  DISPLAY 'SW-IF-NOT-END-OF-SHIP ' SW-IF-NOT-END-OF-SHIP
                  DISPLAY 'WS-ITER-COLUMN-TEMP ' WS-ITER-COLUMN-TEMP    
      * PARAGRAPH WILL CHECK IF IT IS POSSIBLE THAT THIS                
      * FIELD CAN BE A MIDLE FIELD OF A HOIRZONTAL SHIP                 
      * FOR EXAMPLE  --X-- WHERE - REPRESENT VALID NEIGHBORS            
      * AND 'X' RESPRESENTS OUR MIDDLE FIELD                            
      *                                                                 
      * WE JUST HAVE TO CHECK IF ITS TOP AND BOTTOM NEIGHBORS           
      * ARE EMPTY                                                       
                  PERFORM 2052-CHECK-MIDDLE-HORIZONTAL                  
                  IF SO-SHIP-POSITION-VALID THEN                        
                     DISPLAY '2162 AFTER 2052 CALL SO-SHIP-POSITION-VAL'
                     DISPLAY 'ELEMENT HORIZONTAL X: '           
                                                                        
                     WS-ITER-COLUMN-TEMP 'Y: ' WS-ITER-ROW              
                     ADD 1 TO WS-HOW-MANY-ELEMENTS                      
                     SET SO-SHIP-IS-HORIZONTAL  TO TRUE                 
                     DISPLAY ' 2162  WS-HOW-MANY-ELEMENTS '             
                               WS-HOW-MANY-ELEMENTS                     
                     DISPLAY 'SW-IF-SHIP-IS-HORIZONTAL '                
                              SW-IF-SHIP-IS-HORIZONTAL                  
                  ELSE                                                  
      * IF SHIP HAS NEIGHBORS THEN WE WILL NOTIFY THE USER THAT         
      * THIS IS INVALID                                                 
                     DISPLAY '2162 AFTER 2052 SHIP POSITION INVALID '   
                     MOVE 'SHIPS CANT HAVE NIGHBORS' TO MSGO            
                     DISPLAY 'MSGO: ' MSGO                              
                     SET SO-INVALID-USER-SHIPS TO TRUE                  
                     DISPLAY 'SW-IF-VALID-USER-SHIPS  '                 
                     SW-IF-VALID-USER-SHIPS                             
                     PERFORM 2100-SEND-THE-MAP                          
                  END-IF                                                
               ELSE                                                     
      * WE WILL BE HERE IF THIS SHIP IS INVALID OR THIS SHIP IS VALUE   
      * AND PROGRAM JUST FOUND ALL    ITS FIELDS                        
                  DISPLAY '2162 CURRENT FIELD IS NOT "S" '              
                  SET SO-END-OF-SHIP TO TRUE                            
                  DISPLAY 'SW-IF-NOT-END-OF-SHIP  '                     
                     SW-IF-NOT-END-OF-SHIP                              
                  IF  WS-HOW-MANY-ELEMENTS > 1 THEN                     
                    DISPLAY '2162  WS-HOW-MANY-ELEMENTS > 1 '           
      * WE ARE GONNA CHECK FOR LAST HORIZONTAL FIELD OF THIS SHIP       
      *                                                                 
      * WE WILL CHECK IF IT DOES HAVE THE RIGHT NEIGHBOR                
                    PERFORM 2053-CHECK-FOR-LAST-HORIZONTAL              
                    IF SO-SHIP-POSITION-INVALID THEN                    
                       DISPLAY '2162 AFTER 2053 POSITION INVALID '      
                       MOVE 'SHIPS CANT HAVE NIGHBORS' TO MSGO          
                       SET SO-INVALID-USER-SHIPS TO TRUE  
                       DISPLAY '2162 MSGO:  '  MSGO                     
                       DISPLAY ' SW-IF-VALID-USER-SHIPS   '             
                                 SW-IF-VALID-USER-SHIPS                 
                       PERFORM 2100-SEND-THE-MAP                        
                    END-IF                                              
                  END-IF                                                
               END-IF                                                   
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2163-CHECK-VERTICAL-FIELDS                    
      * PARAGRAPH WILL VALIDATE ALL FIELD OF THE VERTICAL SHIP          
      * EXECEPT FOR THE FIRST ONE (THE FIRST FROM THE TOP)              
      *                                                                 
      * IT WILL CHECK IF ANY OF THOSE FIELDS HAS A NEIGHBOR OR ARE      
      * INVALID IN ANY OTHER WAY                                        
      * IF SO IT WILL SET SO-INVALID-USER-SHIPS FLAG TO TRUE            
      *                                                                 
      * IF EVERYTHING GOES WELL THEN THIS VALIDATION WILL               
      * CONFIRM THAT VERTICAL SHIP IS VALID                             
      *                                                                 
      ******************************************************************
       2163-CHECK-VERTICAL-FIELDS.                                      
            DISPLAY ' 2163-CHECK-VERTICAL-FIELDS PERFORMED '            
            IF WS-ITER-ROW-TEMP < 10 AND NOT SO-INVALID-USER-SHIPS      
            THEN                                                        
              ADD 1 TO WS-ITER-ROW-TEMP                                 
              DISPLAY '2163 WS-ITER-ROW-TEMP      '                     
              PERFORM UNTIL SO-END-OF-SHIP OR SO-INVALID-USER-SHIPS     
              OR WS-ITER-ROW-TEMP > 10                                  
                                                                        
                  DISPLAY 'IN 2163 ROW: ' WS-ITER-ROW-TEMP              
                  DISPLAY 'IN 2163 COLUMN: ' WS-ITER-COLUMN             
                IF WS-SCREEN-TABLE(WS-ITER-ROW-TEMP)(WS-ITER-COLUMN:1) =
                     'S' THEN                                           
                   MOVE WS-ITER-ROW-TEMP      TO WS-ROW-VALIDATION      
                   MOVE WS-ITER-COLUMN        TO WS-COLUMN-VALIDATION   
                                                                        
                   DISPLAY '2163  WS-ITER-ROW-TEMP       '              
                                  WS-ITER-ROW-TEMP                      
                   DISPLAY '2163  WS-ITER-COLUMN         '              
                                  WS-ITER-COLUMN                        
                   DISPLAY '2163  WS-ROW-VALIDATION      '              
                                  WS-ROW-VALIDATION                     
                   DISPLAY '2163  WS-COLUMN-VALIDATION   '              
                                  WS-COLUMN-VALIDATION                  
                                                                        
                   PERFORM 2055-CHECK-MIDDLE-VERTICAL                   
                   IF SO-SHIP-POSITION-VALID  THEN                      
                      DISPLAY 'SO-SHIP-POSITION-VALID '                 
                      ADD 1 TO WS-HOW-MANY-ELEMENTS                     
                      SET SO-SHIP-IS-VERTICAL TO TRUE                   
                      DISPLAY ' WS-HOW-MANY-ELEMENTS '                  
                                WS-HOW-MANY-ELEMENTS                    
                      DISPLAY 'SW-IF-SHIP-IS-VERTICAL  '                
                               SW-IF-SHIP-IS-VERTICAL                   
                   ELSE                                                 
                      DISPLAY 'NOT SO-SHIP-POSITION-VALID '             
                                                                        
                      SET SO-INVALID-USER-SHIPS TO TRUE                 
                      MOVE 'SHIPS CANT HAVE NEIGHBORS ' TO MSGO         
                      PERFORM 2100-SEND-THE-MAP                         
                      DISPLAY '2163 MSGO: ' MSGO                        
                      DISPLAY ' SW-IF-VALID-USER-SHIPS  '               
                                SW-IF-VALID-USER-SHIPS                  
                   END-IF                                               
                ELSE                                                    
      * WE WILL BE HERE IF THIS SHIP IS INVALID OR THIS SHIP IS VALUE   
      * AND PROGRAM JUST FOUND ALL OF ITS FIELDS                        
                   DISPLAY '2039 END OF SHIP       '                    
                   SET SO-END-OF-SHIP TO TRUE                           
                   IF WS-HOW-MANY-ELEMENTS > 1 THEN   
                      SUBTRACT 1 FROM WS-ITER-ROW-TEMP                 
                      MOVE WS-ITER-ROW-TEMP TO WS-ROW-VALIDATION       
                      MOVE WS-ITER-COLUMN     TO WS-COLUMN-VALIDATION  
                      PERFORM 2056-CHECK-LAST-VERTICAL                 
                                                                       
                      IF SO-SHIP-POSITION-INVALID THEN                 
                        MOVE 'SHIPS CANT HAVE NIGHBORS' TO MSGO        
                        SET SO-INVALID-USER-SHIPS TO TRUE              
                        PERFORM 2100-SEND-THE-MAP                      
                      END-IF                                           
                   END-IF                                              
                END-IF                                                 
                ADD 1 TO WS-ITER-ROW-TEMP                              
              END-PERFORM                                              
            ELSE                                                       
      * THIS SHIP IS INVALID (THERE IS ONLY ONE FIELD AND              
      * WE CANNOT PLACE ANYTHING VERTICALY THERE  (THERE IS A WALL)    
               SET SO-SHIP-IS-NOT-VERTICAL TO TRUE                     
            END-IF                                                     
           .                                                           
      *****************************************************************
      *                 2164-VALIDATE-LAST-HIT-POS                     
      *****************************************************************
       2164-VALIDATE-LAST-HIT-POS.                                     
           IF NOT SO-RANDOM-SHOT THEN                                  
             MOVE WS-COMPUTER-LAST-ROW-POS    TO WS-ROW-POSITION       
             MOVE WS-COMPUTER-LAST-COLUMN-POS TO WS-COLUMN-POSITION    
             IF WS-ROW-POSITION < 1 OR > 10                            
                OR                                                     
                WS-COLUMN-POSITION < 1 OR > 10 THEN                    
                  DISPLAY 'END DUE TO INVALID DATA '                   
                  DISPLAY 'WS-ROW-POSITION: '    WS-ROW-POSITION       
                  DISPLAY 'WS-COLUMN-POSITION: ' WS-COLUMN-POSITION    
                  PERFORM 3001-ERROR-EXIT                              
             END-IF                                                    
           END-IF    
           .                                                            
      ******************************************************************
      *                    2165-GET-RANDOM-SHOT-POS                     
      * PARAGRAPH WILL GET RANDOM SHOT POSITION                         
      * WE WILL SHOOT THERE LATER                                       
      ******************************************************************
       2165-GET-RANDOM-SHOT-POS.                                        
           DISPLAY 'SO-RANDOM-SHOT'                                     
           SET SO-CONTINUE-WITH-POSITION TO TRUE                        
      * IF LOOP GOES FOR THE FIRST TIME WE HAVE TO PROVIDE SEED         
      * FOR THE RANDOM FUNCTION                                         
      * BUT IF THIS LOOP GOES FOR NOT FIRST TIME THEN WE ALREADY DONE   
      * THAT - SO THERE IS NO NEED TO DO THAT ONCE AGAIN                
           IF WS-ITERX  = 1                                             
               DISPLAY 'WS-ITERX = 1 '                                  
               PERFORM 2007-GET-FIRST-SEED                              
           ELSE                                                         
               COMPUTE WS-RANDOM-VALUE = FUNCTION RANDOM * 1000000      
               COMPUTE WS-RANDOM-VALUE = WS-RANDOM-VALUE / 100          
               DISPLAY '2165 WS-RANDOM-VALUE  = : ' WS-RANDOM-VALUE     
           END-IF                                                       
           PERFORM 2010-CALCULATE-POSITION                              
           MOVE WS-ROW-POSITION    TO WS-COMPUTER-LAST-ROW-POS          
           MOVE WS-COLUMN-POSITION TO WS-COMPUTER-LAST-COLUMN-POS       
           DISPLAY '2165 POSITIONS: '                                   
           DISPLAY '2165 ROW: '      WS-COMPUTER-LAST-ROW-POS           
           DISPLAY '2165 COLUMN: '   WS-COMPUTER-LAST-COLUMN-POS        
                                                                        
                                                                        
                                                                        
                                                                        
           .                                                            
      ******************************************************************
      *                 2166-DECIDE-WHERE-TO-SHOT-NEXT                  
      * PARAGRAPH WILL DECIDE WHAT TO DO AFTER TAKING AND               
      * SUCCESSFULL SHOT                 
      *                                                                 
      * IF COMPUTER'S SHOT DESTROEYD THE SHIP THEN NEXT SHOT            
      * WILL BE RANDOM ONE                                              
      *                                                                 
      * IF COMPUTER'S SHOT MISSED THE SHIP ( BUT EARILER IT HIT A       
      * SHIP THEN WE WILL PREPARE A NEXT SHOT ) - PARAGRAPH 2140 WILL   
      * BE PERFORMED AND THIS PARAGRAPH WILL DETERMINE WHERE TO SHOT    
      *                                                                 
      * IF COMPUTER''S SHOT MISSED A SHIP AND EARILER COMPUTER DIDNT'   
      * HIT  A SHIP EITHER THEN NEXT SHOT WILL BE RANDOM                
      *                                                                 
      * IF COMPUTER HIT A SHIP BUT IT WAS NOT DESTORYED THEN            
      * PARAGRAPH 2140 WILL BE PERFORMED AND IT WILL LOOK FOR THE NXT   
      * TARGET                                                          
      ******************************************************************
       2166-DECIDE-WHERE-TO-SHOT-NEXT.                                  
           EVALUATE TRUE                                                
              WHEN SO-DESTROYED-SHOT                                    
      * IF COMPUTER DESTROEYD THE WHOLE SHIP THEN THE NEXT SHOOT        
      * WILL BE RANDOMIZED                                              
                  DISPLAY '2112 SO-DESTROYED SHOT '                     
                  SET SO-RANDOM-SHOT TO TRUE                            
              WHEN SO-MISSED-SHOT                                       
      * IF COMPUTER DIDN'T HIT OR DESTROY ANY SHIP THEN WE WILL         
      * RANDOMIZE NEXT SHOT                                             
                  DISPLAY '2112 SO-MISSED    SHOT '                     
      * THOSE 2 IF STATEMENTS WILL PREPARE THE NEXT SHOT                
                                                                        
                                                                        
                  MOVE WS-COMPUTER-LAST-ROW-POS TO WS-ROW-POSITION      
                  MOVE WS-COMPUTER-LAST-COLUMN-POS                      
                                     TO WS-COLUMN-POSITION              
                  IF NOT SO-RANDOM-SHOT THEN                            
                     PERFORM 2140-PREPARE-NEXT-SHOT                     
                  END-IF                                                
              WHEN SO-HIT-SHOT 
      * WE ALREADY KNOW WHAT IS THE ORIENTATION OF THIS                 
      * SHIP                                                            
                   DISPLAY '2112 SO-HIT    SHOT '                       
                   PERFORM 2140-PREPARE-NEXT-SHOT                       
                   MOVE WS-ROW-POSITION    TO WS-COMPUTER-LAST-ROW-POS  
                   MOVE WS-COLUMN-POSITION                              
                                          TO WS-COMPUTER-LAST-COLUMN-POS
                                                                        
              WHEN OTHER                                                
                   CONTINUE                                             
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                2169-CHECK-IF-VALID-CHARACTERS.                  
      * PARAGRAPH WILL CHECK ALL FIELDS ON THE USER'S BOARD             
      * AND WILL VALIDATE IF THEY ARE VALID SYMBOL 'S' OR SPACE OR LOW  
      * VALUES                                                          
      ******************************************************************
       2169-CHECK-IF-VALID-CHARACTERS.                                  
           DISPLAY '2169-CHECK-IF-VALID-CHARACTERS PERFORMED'           
           SET SO-VALID-USER-SHIPS   TO TRUE                            
           PERFORM VARYING WS-ITER-COLUMN FROM 1 BY 1 UNTIL             
                  WS-ITER-COLUMN > CT-MAXIMAL-WIDTH-OF-BOARD            
                  OR SO-INVALID-USER-SHIPS                              
                                                                        
                 PERFORM VARYING WS-ITER-ROW FROM 1 BY 1 UNTIL          
                  WS-ITER-ROW > CT-MAXIMAL-HEIGHT-OF-BORAD              
                  OR SO-INVALID-USER-SHIPS                              
                  IF WS-SCREEN-TABLE(WS-ITER-ROW)(WS-ITER-COLUMN:1) =   
                             SPACE OR 'S' OR LOW-VALUES  THEN           
                      DISPLAY 'VALID SYMBOL ON THE MAP  NO ACTION TAKEN'
                  ELSE                                                  
                      MOVE 'INVALID SYMBOL ON THE BOARD '               
                       TO MSGO                                          
                      SET SO-INVALID-USER-SHIPS TO TRUE                 
                      DISPLAY 'MSGO:  ' MSGO     
                      DISPLAY ' SW-IF-VALID-USER-SHIPS '                
                              SW-IF-VALID-USER-SHIPS                    
                  END-IF                                                
                 END-PERFORM                                            
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                  2170-SEND-SINGLE-MAP-CURSOR                    
      ******************************************************************
       2170-SEND-SINGLE-MAP-CURSOR.                                     
           DISPLAY '2170 SEND CURSOR WAS PERFORMED '                    
           DISPLAY '----------------------2170 SEND THE MAP '           
           DISPLAY 'POLEUO(1) '  POLEUO(1)                              
           DISPLAY 'POLEUO(2) '  POLEUO(2)                              
           DISPLAY 'POLEUO(3) '  POLEUO(3)                              
           DISPLAY 'POLEUO(4) '  POLEUO(4)                              
           DISPLAY 'POLEUO(5) '  POLEUO(5)                              
           DISPLAY 'POLEUO(6) '  POLEUO(6)                              
           DISPLAY 'POLEUO(7) '  POLEUO(7)                              
           DISPLAY 'POLEUO(8) '  POLEUO(8)                              
           DISPLAY 'POLEUO(9) '  POLEUO(9)                              
           DISPLAY 'POLEUO(10) ' POLEUO(10)                             
           EXEC CICS                                                    
             SEND MAP('MP0234') MAPSET('MP0234')                        
             FROM(MP0234O)                                              
             CURSOR(WS-LAST-EIBCPOSN)                                   
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *               2171-PREP-NEIGHBOURS-CHECK                        
      * WS-MAIN-FIELD-ROW  & WS-MAIN-FIELD-COLUMN WILL STORE            
      * ROW AND COLUMN FOR THE MAIN FIELD (THE FIELD THAT IS IN CENTER  
      * AND THIS PROGRAM IS CHECKING ITS NEIGHBORS)                     
      *                                                
      * ( WE KNOT THAT "MAIN" FIELD WAS HIT (THERE IS A NOT DESTORYED   
      * SHIP)                                                           
      ******************************************************************
       2171-PREP-NEIGHBOURS-CHECK.                                      
           MOVE  WS-ROW-POSITION        TO  WS-MAIN-FIELD-ROW           
           MOVE  WS-COLUMN-POSITION     TO  WS-MAIN-FIELD-COLUMN        
                                                                        
           MOVE WS-ROW-POSITION         TO  WS-TEMP-ROW                 
           MOVE WS-COLUMN-POSITION      TO  WS-TEMP-COLUMN              
           SET  SO-OPPOSITE-IS-INVALID  TO TRUE                         
           .                                                            
      ******************************************************************
      *                   2182-CHECK-OPPOSITE-WHEN-HIT                  
      *                                                                 
      * AT THE BEGINIG PARAGRAPH WILL CALCULATE ROW AND COLUMN NUMBER   
      * OF THE FIELD THAT IS THE OPPOSITE FIELD TO WHAT WE ARE NOW      
      * CHECKING                                                        
      *                                                                 
      ******************************************************************
       2182-CHECK-OPPOSITE-WHEN-HIT.                                    
           DISPLAY '2182 CHECK WHEN HIT PERFORMED'                      
           COMPUTE WS-ROW-OFFSET  =  WS-MAIN-FIELD-ROW - WS-TEMP-ROW    
           COMPUTE WS-OPPOSITE-ROW  =  WS-MAIN-FIELD-ROW                
                                                         + WS-ROW-OFFSET
           COMPUTE WS-COLUMN-OFFSET  = WS-MAIN-FIELD-COLUMN -           
                                                          WS-TEMP-COLUMN
           COMPUTE WS-OPPOSITE-COLUMN = WS-MAIN-FIELD-COLUMN +          
                                                        WS-COLUMN-OFFSET
                                                                        
           IF WS-OPPOSITE-ROW >= 1 AND <= 10                            
                      AND                                               
              WS-OPPOSITE-COLUMN >= 1 AND <= 10 THEN                    
      * IF "OPPOSITE FIELD" IS A VALID FIELD (IT CAN BE ON THE BOARD)   
      * THEN WE WILL CHECK WHAT IS ON THIS POSITION                     
           DISPLAY '2182 OPPOSITE IS VALID '                            
           SET SO-OPPOSITE-IS-VALID TO TRUE  
           MOVE WS-PROGRAM-ARRAY(WS-OPPOSITE-ROW)(WS-OPPOSITE-COLUMN:1) 
            TO  SW-WHAT-TYPE-OF-FIELD                                   
            EVALUATE TRUE                                               
            WHEN SO-HIT-FIELD                                           
      * IF THE "OPPOSITE" WAS HIT THEN WE CAN BE IN SITUATION LIKE      
      * THIS                                                            
      *            -             WHERE :                                
      *            H                                                    
      *            X                - (EMPTY OR UNKNOWN FIELD)          
      *                             H (OUR MAIN FIELD)                  
      *                             X (HIT FIELD (OUR OPPOSITE))        
      *                                                                 
      *    IF THIS IS THE CASE THEN PROGRAM CAN SHOOT UP OR DOWN        
      *  (LEFT OR RIGHT IN HORIZONTAL SHIP)                             
      *                                                                 
      *  SO WE WILL CHOOSE RANDOMLY WHERE TO SHOOT                      
      *                                                                 
      * WE CAN SHOOT IN "OPPOSITE" DIRECTION ( IN THE DIRETION OF THE   
      * OPPOSITE FIELD) OR TO "LAST HIT" DIRECTION SO IN                
      * 'H' DIRECTION                                                   
      *                                                                 
      *                                                                 
               DISPLAY '2182 OPPOSIDTE IS HIT SO RANDOM DIRECTION '     
               PERFORM 2183-GET-A-RANDOM-DIRECTION                      
            WHEN SO-MISSED-FIELD                                        
      *  HERE WE ARE IN THE SITUATION LIKE THIS:                        
      *                                                                 
      *             -              O (REPRESENT A MISS)                 
      *             H                                                   
      *             O                                                   
      *                                                                 
      *  IF THIS IS THE CASE WE WILL SHOT IN THE 'H' DIRECTION          
      *                                                                 
      *                                                                 
                   DISPLAY '2182 OPPOSIDTE IS MISS SOLST HIT DIRECTION '
                   SET SO-SHOT-IN-LAST-HIT-DIRECTION TO TRUE    
            WHEN SO-EMPTY-FIELD OR SO-SHIP-FIELD                        
      * IF THE "OPPOSITE" FIELD IS EMPTY OR THERE IS A SHIP THEN        
      * WE WILL SHOOT IN THIS DIRECTION                                 
      * 'OPPOSITE DIRECTION MEANS THE DIRECTION OF THE OPPOSITE FIELD'  
                                                                        
                   DISPLAY '2182 OPPOSIDTE IS EMPTY SO OPPOSITE DIR '   
                   SET SO-SHOT-IN-OPPOSITE-DIRECTION TO TRUE            
            WHEN OTHER                                                  
      * IF WE ENCOUNTER ANY OTHER FIELD IT MEANS AN ERROR               
      *                                                                 
      * WE CANNOT HAVE 'Z' AS OUR NEIGHBOR SO IT IS NOT IN THIS         
      * EVALUTE SATEMENT ( IT IS NOT POSSIBLE TO ENCOUNTER IT HERE )    
      * IF THERE IS ANY OTHER NOT DEFINED SYMBOL ON THE MAP THEN        
      * WE WILL TERMINATE THIS TRANSACTION ABNORMALLY                   
               DISPLAY '2182 OTHER IN EVALUATE '                        
               DISPLAY 'SW-WHAT-TYPE-OF-FIELD: ' SW-WHAT-TYPE-OF-FIELD  
               PERFORM 3001-ERROR-EXIT                                  
            END-EVALUATE                                                
           ELSE                                                         
      * IF "OPPOSITE FIELD" IS INVALID (IT IS NOT A PLACE ON THE BOARD )
      * THEN WE WILL SET THIS FLAG TO TRUE                              
              SET SO-OPPOSITE-IS-INVALID TO TRUE                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2183-GET-A-RANDOM-DIRECTION                    
      ******************************************************************
       2183-GET-A-RANDOM-DIRECTION.                                     
           PERFORM 2173-GET-THE-SEED                                    
           COMPUTE WS-RANDOM-VALUE = FUNCTION RANDOM(WS-FIRST-SEED)     
             * 2 + 1                                                    
      * PARAGRAPH WILL SET A CORRECT FLAG TO TRUE DEPENDING ON          
      * WHAT WILL BE THE VALUE OF WS-RANDOM-VALUE VARRIABLE             
      * IN CASE THAT WE WILL GET VALUE DIFFERENT THAN 1 OR 2 THEN       
      * WE WILL SHOT IN THE LAST HIT DIRECTION                          
           EVALUATE WS-RANDOM-VALUE
           WHEN 1                                                       
              DISPLAY '2183  SHOOT IN OPPOSITE'                         
              SET SO-SHOT-IN-OPPOSITE-DIRECTION TO TRUE                 
           WHEN 2                                                       
              DISPLAY '2183  SHOOT IN LAST HIT DIR'                     
              SET SO-SHOT-IN-LAST-HIT-DIRECTION TO TRUE                 
           WHEN OTHER                                                   
              DISPLAY '2183  SHOOT IN LAST HIT DIR'                     
              SET SO-SHOT-IN-LAST-HIT-DIRECTION TO TRUE                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2185-GET-OPPOSITE-DIRECTION                   
      * PARAGRAPH WILL GET DIRECTION OF THE OPPOSITE FIELD              
      ******************************************************************
       2185-GET-OPPOSITE-DIRECTION.                                     
           EVALUATE TRUE                                                
           WHEN WS-ROW-OFFSET = 0 AND WS-COLUMN-OFFSET = 1              
                SET SO-RIGHT-SHOT TO TRUE                               
           WHEN WS-ROW-OFFSET = 1 AND WS-COLUMN-OFFSET = 0              
                SET SO-BOTTOM-SHOT TO TRUE                              
           WHEN WS-ROW-OFFSET = 0 AND WS-COLUMN-OFFSET = -1             
                SET SO-LEFT-SHOT TO TRUE                                
           WHEN WS-ROW-OFFSET = -1 AND WS-COLUMN-OFFSET = 0             
                SET SO-TOP-SHOT  TO TRUE                                
           WHEN OTHER                                                   
               DISPLAY 'OTHER EVALUATE IN 2185 '                        
               MOVE WS-ROW-OFFSET    TO WS-ROW-OFFSET-FORMAT            
               MOVE WS-COLUMN-OFFSET TO WS-COLUMN-OFFSET-FORMAT         
               DISPLAY ' WS-ROW-OFFSET: '   WS-ROW-OFFSET-FORMAT        
               DISPLAY 'WS-COLUMN-OFFSET: ' WS-COLUMN-OFFSET-FORMAT     
               PERFORM 3001-ERROR-EXIT                                  
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2186-GET-LAST-HIT-DIRECTION     
      * PARAGRAPH WILL GET DIRECTION OF THE LAST HIT FIELD              
      ******************************************************************
       2186-GET-LAST-HIT-DIRECTION.                                     
           EVALUATE TRUE                                                
           WHEN WS-ROW-OFFSET = 0 AND WS-COLUMN-OFFSET = 1              
                SET SO-LEFT-SHOT TO TRUE                                
           WHEN WS-ROW-OFFSET = 1 AND WS-COLUMN-OFFSET = 0              
                SET SO-TOP-SHOT TO TRUE                                 
           WHEN WS-ROW-OFFSET = 0 AND WS-COLUMN-OFFSET = -1             
                SET SO-RIGHT-SHOT TO TRUE                               
           WHEN WS-ROW-OFFSET = -1 AND WS-COLUMN-OFFSET = 0             
                SET SO-BOTTOM-SHOT  TO TRUE                             
           WHEN OTHER                                                   
               DISPLAY 'OTHER EVALUATE IN 2186 '                        
               DISPLAY ' WS-ROW-OFFSET: ' WS-ROW-OFFSET                 
               DISPLAY 'WS-COLUMN-OFFSET: ' WS-COLUMN-OFFSET            
               PERFORM 3001-ERROR-EXIT                                  
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    2187-GET-SHOT-DIRECTION                      
      * PARAGRAPH IS CALLED WHILE PROGRAM WANTS TO DETEMINE             
      * IN WHICH DIRECTION IT SHOULD SHOOT NEXT                         
      *                                                                 
      * PROGRAM WILL GET HERE IN ORDER TO GET INFO                      
      * ABOUT THE DIRECTION WHERE IT SHOULD SHOOT                       
      *                                                                 
      *  2185-GET-OPPOSTIE-DIRECTION WILL PROVIDE US WITH DIRECTION     
      * OF THE 'OPPOSITE FIELD' OPPOSITE FIELD IS A FIELD THAT          
      * IS AT THE OPPOSITE SIDE OF THE FIELD WE ARE CURRENTLY CHECKING  
                                                                        
      *   2186-GET-LAST-HIT-DIRECTION WILL GET AS THE DIRECTION OF      
      * THE FIELD THAT WAS HIT PREVIOUSLY                               
      ******************************************************************
       2187-GET-SHOT-DIRECTION.                                         
           EVALUATE TRUE    
           WHEN SO-SHOT-IN-OPPOSITE-DIRECTION                           
              PERFORM 2185-GET-OPPOSITE-DIRECTION                       
           WHEN SO-SHOT-IN-LAST-HIT-DIRECTION                           
              PERFORM 2186-GET-LAST-HIT-DIRECTION                       
           WHEN OTHER                                                   
             DISPLAY 'OTHER IN EVALUATE IN 2187 '                       
             PERFORM 3001-ERROR-EXIT                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2188-DECIDE-WHERE-TO-SHOOT                     
      ******************************************************************
       2188-DECIDE-WHERE-TO-SHOOT.                                      
           DISPLAY '2188 WS-PROGRAM-ARRAY '                             
            WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1)             
           IF WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1)  = 'X'    
             OR ' ' OR LOW-VALUES  OR 'S'                               
             DISPLAY '2188 JEST HIT ALBO PUSTO '                        
             PERFORM 2182-CHECK-OPPOSITE-WHEN-HIT                       
                                                                        
      * THE PARAGRAPH ABOVE WILL CHOOSE IF PROGRAM SHOOLD SHOOT IN THE  
      * LAST HIT DIRECTION = DIRECTION OF THE LAST HIT FIELD            
      * OF TO THE OPPOSITE DIRECTION ( IF WE ARE CHECKING TOP FILED     
      * ABOVE THE LAST HIT THEN THE OPPOSITE DIRECTION WILL BE AT THE   
      * BOTTOM OF THE MAIN FIELD)                                       
             IF SO-OPPOSITE-IS-VALID THEN                               
      * THIS PARAGRAPH WILL GET ACTUAL DIRECTION WHERE PROGRAM          
      * SHOULD SHOT (TOP,BOTTOM,LEFT OR RIGHT)                          
                 PERFORM 2187-GET-SHOT-DIRECTION                        
             ELSE                                                       
      * IF SO-OPPOSITE-IS-INVALID FLAG IS SET TO TRUE                   
      * IT MEANS THAT IT IS PHYSICLY IMPOSIBLE TO FIRE AT THE           
      * "OPPOSITE DIRECTION" BECAUSE WE ARE AT THE WALL                 
      * WHEN THAT IS THE CASE THEN WE WILL SHOT IN THE LAST             
      * HIT DIRECTION                                                   
                 SET SO-OPPOSITE-IS-VALID TO TRUE       
                 PERFORM 2186-GET-LAST-HIT-DIRECTION                    
                                                                        
             END-IF                                                     
                                                                        
           ELSE                                                         
      * IF THERE IS ANY OTHER SYMBOL ON THE BOARD OTHER THATN           
      * 'X' ' ' 'S' OR LOW-VALUES WE WILL SET THE FLAG BELOW AS         
      * THE INVALID OPPOSITE THAT IS BECAUSE WE WONT TO NOT ALLOW       
      * TO THE SITUATION THAT PROGRAM FIRES IN THE WRONG DIRECTIONS     
      * FOR EXMAPLE IN CASE THAT THERE IS A MISS ABOVE OUR MAIN FIELD   
      * WE WILL DO NOTHING, (WE WILL START CHECKING OTHER DIRECTION)    
      * TO THE MOMENT WE WILL FIND A EMPTY FIELD, SHIP FIELD OR HIT     
      * FIELD IN ANY OF THOSE DIRECTIONS AND THEN WE WILL DECIDE WHAT   
      * TO DO                                                           
             SET SO-OPPOSITE-IS-INVALID TO TRUE                         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2190-GET-RANDOM-SIDE                         
      * THIS PARAGRAPH WILL RETURN WS-RANDOM-VALUE THAT WILL            
      * BE ALWAYS FROM A RANGR FROM 1 TO 4                              
      *                                                                 
      ******************************************************************
       2190-GET-RANDOM-SIDE.                                            
           COMPUTE WS-RANDOM-VALUE = FUNCTION RANDOM * 1000000          
           COMPUTE WS-RANDOM-VALUE = WS-RANDOM-VALUE / 1000             
           IF WS-RANDOM-VALUE > 4 THEN                                  
             SUBTRACT 5 FROM WS-RANDOM-VALUE                            
           END-IF                                                       
      * THIS SHOULD NOT HAPPEN BUT JUST IN CASE IF ANYTHING GOES        
      * WRONG PROGRAM WILL NOT ALLOW FOR INVALID DATA                   
           DISPLAY '2190 GET RANDOM-VALUE : ' WS-RANDOM-VALUE           
           IF WS-RANDOM-VALUE EQUAL 1 OR 2 OR 3 OR 4 THEN               
             CONTINUE                                                   
           ELSE                                                         
             DISPLAY 'IN IF STATEMENT IN 2190 RANDOM: ' WS-RANDOM-VALUE 
             MOVE 1 TO WS-RANDOM-VALUE                                  
           END-IF                                                       
                                                                        
                                                                        
      * PARAGRAPH WILL CHOOSE ONE OUT OF 4 POSSIBLE SIDES DEPENDING ON  
      * WHAT IS THE RANDOM-VALUE GENERATED BY THE RANDOM FUNCTION       
      * IF WS-RANDOM-VALUE WILL BE EQUAL TO ONE THEN NEXT OUR SIDE IS   
      * RIGHT                                                           
      *                                                                 
      * IF WS-RANDOM-VALUE WILL BE EQUAL TO TWO THEN NEXT OUR SIDE IS   
      * LEFT                                                            
      *                                                                 
      * IF WS-RANDOM-VALUE WILL BE EQUAL TO 3 THEN NEXT OUR SIDE IS     
      * A TOP                                                           
      *                                                                 
      * IF WS-RANDOM-VALUE WILL BE EQUAL TO 4 THEN NEXT OUR SIDE IS     
      * A BOTTOM                                                        
      *                                                                 
      * PROGRAM WILL ONLY CHOOSE A SIDE WHERE IT IS POSSIBLE TO SHOOT   
      * SO THERE IS ADDITIONAL CHECK IN EACH OF THE WHEN STAEMENT THAT  
      * CONFIRMES THAT THE SIDE IS VALID                                
      * FOR EXAMPLE WHEN WE WANT TO SHOOT AT THE RIGHT SIDE             
      * OUR COLUMN NUMBER HAS TO BE LESS THAN 10 BECAUSE 10 IS          
      * THE MAXIMAL WIDTH OF THE USER BOARD ( AND WE CANT SHOT AT       
      * THE RIGHT)                                                      
           DISPLAY 'WS-RANDOM-VALUE : ' WS-RANDOM-VALUE                 
           EVALUATE TRUE                                                
      * IF RANDOM RETURNED VALUE OF '1' THEN WE WILL SHOT AT RIGHT      
           WHEN SO-RIGHT-SIDE-CHOSEN AND WS-TEMP-COLUMN <               
                                           CT-MAXIMAL-WIDTH-OF-BOARD    
             SET SO-SIDE-IS-VALID   TO TRUE                             
             SET SO-RIGHT-SHOT      TO TRUE                             
      * IF RANDOM RETURNED VALUE OF '2' THEN WE WILL SHOT AT LEFT       
           WHEN SO-LEFT-SIDE-CHOSEN AND WS-TEMP-COLUMN > 1              
             SET SO-SIDE-IS-VALID   TO TRUE                             
             SET SO-LEFT-SHOT       TO TRUE  
      * IF RANDOM RETURNED VALUE OF '3' THEN WE WILL SHOT AT TOP        
           WHEN SO-TOP-SIDE-CHOSEN  AND WS-TEMP-ROW > 1                 
             SET SO-SIDE-IS-VALID   TO TRUE                             
             SET SO-TOP-SHOT        TO TRUE                             
      * IF RANDOM RETURNED VALUE OF '4' THEN WE WILL SHOT TO THE BOTTOM 
           WHEN SO-BOTTOM-SIDE-CHOSEN  AND WS-TEMP-ROW <                
                                     CT-MAXIMAL-HEIGHT-OF-BORAD         
             SET SO-SIDE-IS-VALID   TO TRUE                             
             SET SO-BOTTOM-SHOT     TO TRUE                             
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                 2192-WHAT-IF-EMPTY-NEIGHBOR                     
      ******************************************************************
       2192-WHAT-IF-EMPTY-NEIGHBOR.                                     
           DISPLAY '2192 PERFORMED'                                     
           IF WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) = ' ' OR  
             LOW-VALUES OR 'S'                                          
                                                                        
           THEN                                                         
             DISPLAY '2192 PERFORMED  VALID '                           
             COMPUTE WS-ROW-OFFSET  =  WS-MAIN-FIELD-ROW - WS-TEMP-ROW  
             COMPUTE WS-COLUMN-OFFSET  = WS-MAIN-FIELD-COLUMN -         
                                                          WS-TEMP-COLUMN
             PERFORM 2186-GET-LAST-HIT-DIRECTION                        
             SET SO-OPPOSITE-IS-VALID TO TRUE                           
           ELSE                                                         
             DISPLAY '2192 PERFORMED  INVALID '                         
             SET SO-OPPOSITE-IS-INVALID TO TRUE                         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2193-CHECK-OPPOSITE-WHEN-EMPTY               
      ******************************************************************
       2193-CHECK-OPPOSITE-WHEN-EMPTY.                                  
           DISPLAY '2193 WHEN EMPTY FIELD '     
           COMPUTE WS-ROW-OFFSET  =  WS-MAIN-FIELD-ROW - WS-TEMP-ROW    
           COMPUTE WS-OPPOSITE-ROW  =  WS-MAIN-FIELD-ROW                
                                                         + WS-ROW-OFFSET
           COMPUTE WS-COLUMN-OFFSET  = WS-MAIN-FIELD-COLUMN -           
                                                          WS-TEMP-COLUMN
           COMPUTE WS-OPPOSITE-COLUMN = WS-MAIN-FIELD-COLUMN +          
                                                        WS-COLUMN-OFFSET
           DISPLAY '2193  ROW AND COLUMN   :  '                         
           DISPLAY '2193 ROW (OPPOSITE)    :  '  WS-OPPOSITE-ROW        
           DISPLAY '2193 COLUMN (OPPOSITE) :  ' WS-OPPOSITE-COLUMN      
           DISPLAY '2193 ROW (MAIN )       :  ' WS-MAIN-FIELD-ROW       
           DISPLAY '2193 COLUMN (MAIN)     :  ' WS-MAIN-FIELD-COLUMN    
           DISPLAY '2193 ROW (TEMP )       :  ' WS-TEMP-ROW             
           DISPLAY '2193 COLUMN (TEMP)     :  ' WS-TEMP-COLUMN          
           IF WS-OPPOSITE-ROW >= 1 AND <= 10                            
                      AND                                               
              WS-OPPOSITE-COLUMN >= 1 AND <= 10 THEN                    
                                                                        
             SET SO-OPPOSITE-IS-VALID TO TRUE                           
             DISPLAY '2193 SO OPPOSITE VALID  '                         
            MOVE WS-PROGRAM-ARRAY(WS-OPPOSITE-ROW)(WS-OPPOSITE-COLUMN:1)
             TO  SW-WHAT-TYPE-OF-FIELD                                  
            EVALUATE TRUE                                               
              WHEN SO-MISSED-FIELD                                      
                    DISPLAY '2193 MISSED (OPOSITE INVALID) '            
                    SET SO-OPPOSITE-IS-INVALID TO TRUE                  
              WHEN SO-SHIP-FIELD  OR SO-EMPTY-FIELD                     
                    DISPLAY '2193 EMPTY OR SHIP (SHOOT OPPOSITE '       
                    SET SO-SHOT-IN-OPPOSITE-DIRECTION TO TRUE           
              WHEN OTHER                                                
                DISPLAY '2193 EVALUATE (WRONG )'                        
                DISPLAY 'SW-WHAT-TYPE-OF-FIELD: ' SW-WHAT-TYPE-OF-FIELD 
                PERFORM 3001-ERROR-EXIT                                 
            END-EVALUATE                                                
           ELSE                                                         
              DISPLAY ' 2193 OPPOSITE FIELD IS INVALID '
              SET SO-OPPOSITE-IS-INVALID TO TRUE                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2195-CHECK-IF-HIT-NEIGHBOR                   
      * PARAGRAPH WILL PERFORM THE LOOP THAT WILL PERFORM               
      * ANOTHER LOOP                                                    
      * THIS INNER LOOP WILL CHECK NEIGHBORS HORIZONTALY (LEFT AND      
      * RIGHT) AND THEN VERTICALY (TOP AND BOTTOM)                      
      *                                                                 
      ******************************************************************
       2195-CHECK-IF-HIT-NEIGHBOR.                                      
           DISPLAY '2195 PERFORMED '                                    
           MOVE WS-ROW-POSITION     TO WS-TEMP-ROW                      
           MOVE WS-COLUMN-POSITION  TO WS-TEMP-COLUMN                   
                                                                        
           SET SO-HORIZONTAL-CHECK TO TRUE                              
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 2      
             DISPLAY 'IN LOOP OF 2195'                                  
             PERFORM 2205-CHECK-NEIGHBOURS                              
             SET SO-VERTICAL-CHECK TO TRUE                              
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                   2198-CHOOSE-RANDOM-SIDE                       
      * IN CASE THAT THERE WAS NO OTHER VALID DIRECTION OF THE SHOOT    
      * THEN WE WILL CHOOSE A VALID SIDE RANDOMLY                       
      *                                                                 
      *                                                                 
      ******************************************************************
       2198-CHOOSE-RANDOM-SIDE.                                         
           MOVE WS-ROW-POSITION      TO WS-TEMP-ROW                     
           MOVE WS-COLUMN-POSITION   TO WS-TEMP-COLUMN                  
           IF SO-OPPOSITE-IS-INVALID THEN                               
             DISPLAY 'SO OPOSITE IS INVALID WYBIERAMY RANDOM '          
             SET SO-SIDE-IS-INVALID TO TRUE   
                MOVE 0 TO WS-ITER5                                      
             DISPLAY 'PRZED PETLA'                                      
             PERFORM 2173-GET-THE-SEED                                  
             PERFORM UNTIL SO-SIDE-IS-VALID OR WS-ITER5 > 10            
                ADD 1   TO WS-ITER5                                     
                PERFORM 2190-GET-RANDOM-SIDE                            
                                                                        
             END-PERFORM                                                
             IF WS-ITER5 > 10 THEN                                      
               DISPLAY 'PRZEBICIE WS-ITER5  '                           
               PERFORM 3001-ERROR-EXIT                                  
             END-IF                                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2172-GET-A-RANDOM-SIDE                       
      * THIS PARAGRAPH WILL GENERATE A RANDOM NUMBER                    
      * BETWEEN 1 AND 4 AND BASED ON THAT RANDOM VALUE                  
      * PROGRAM WILL CHOOSE A SIDE WHERE COMPUTER SHOULD SHOT NEXT      
      ******************************************************************
       2172-GET-A-RANDOM-SIDE.                                          
           PERFORM 2173-GET-THE-SEED                                    
           COMPUTE WS-RANDOM-VALUE = FUNCTION RANDOM(WS-FIRST-SEED)     
            * 4 + 1                                                     
      * THIS IF STATEMENT SHOULD NEVER BE TRUE                          
      * BUT JUST IN CASE IT WILL NOT ALLOW PROGRAM TO BRAKE             
           IF WS-RANDOM-VALUE NOT = 1 OR 2 OR 3 OR 4 THEN               
              MOVE 1 TO WS-RANDOM-VALUE                                 
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2173-GET-THE-SEED                            
      ******************************************************************
       2173-GET-THE-SEED.                                               
           DISPLAY '2173-GET-THE-SEED  PERFORMED '                      
           EXEC CICS      
             ASKTIME ABSTIME(WS-TEMP-VALUE)                             
           END-EXEC                                                     
           EXEC CICS                                                    
             FORMATTIME                                                 
             ABSTIME(WS-TEMP-VALUE)                                     
             TIME(WS-TIME-VALUE)                                        
             TIMESEP(':')                                               
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
                                                                        
                                                                        
                                                                        
           IF FUNCTION TEST-NUMVAL(WS-TIME-VALUE(7:2)) = 0 THEN         
             COMPUTE WS-TEMP-VALUE2 = FUNCTION NUMVAL(                  
                           WS-TIME-VALUE(7:2))                          
             DISPLAY 'WS-TEMP-VALUE = ' WS-TEMP-VALUE2                  
             COMPUTE WS-FIRST-SEED = WS-TEMP-VALUE2 * 255               
             DISPLAY ' 2007WS-FIRST-SEED: '  WS-FIRST-SEED              
           ELSE                                                         
             MOVE 'ERROR IN THE OMPUTE STATEMENT' TO MSGO               
             DISPLAY '2173 MSGO: ' MSGO                                 
             PERFORM 2100-SEND-THE-MAP                                  
           END-IF                                                       
                                                                        
                                                                        
                                                                        
           .                                                            
      ******************************************************************
      *                 2175-CHECK-RANDOM-NEIGHBORS                     
      ******************************************************************
      *2175-CHECK-RANDOM-NEIGHBORS2.                                    
      *    DISPLAY '2175 WAS PERFORMED'                                 
      *    SET SO-CONTINUE-WITH-POSITION TO TRUE                        
      *    PERFORM 2176-CHECK-RANDOM-RIGHT-SIDE                         
      *    PERFORM 2177-CHECK-RANDOM-LEFT-SIDE                          
      *    PERFORM 2178-CHECK-RANDOM-UPPER    
      *    PERFORM 2179-CHECK-RANDOM-LOWER-SIDE                         
      *    .                                                            
      ******************************************************************
      *                    2175-CHECK-RANDOM-NEIGHBORS                  
      * PARAGAPH WILL BE CALLED WHEN COMPUTER HAS TO GET RANDOM         
      * BOARD POSITION IN ORDER TO SHOOT THERE                          
      * THIS PARAGRAPH WILL CHECK IF IN THE NEIGHBOURHOOD OF THE        
      * NEW POSITION IS ANY 'Z' -> 'Z' REPREZENS DESTROEYD SHIP         
      * IF THIS IS TRUE, THEN WE WILL FIND ANOUTHER POSITION            
      * BECAUSE THIS IS IMPOSSIBLE TO FIND ANY SHIP THERE (             
      * BECAUSE THEY CANT HAVE NEIGHBORS)                               
      ******************************************************************
       2175-CHECK-RANDOM-NEIGHBORS.                                     
           MOVE WS-COLUMN-POSITION            TO WS-COLUMN-CHECK-POS    
           MOVE WS-ROW-POSITION               TO WS-ROW-CHECK-POS       
           SET SO-CONTINUE-WITH-POSITION      TO TRUE                   
                                                                        
           PERFORM VARYING WS-COLUMN-ITERATOR FROM -1 BY 1 UNTIL        
               WS-COLUMN-ITERATOR > 1 OR  SO-NOT-CONTINUE-WITH-POSITION 
                                                                        
               PERFORM VARYING WS-ROW-ITERATOR FROM -1 BY 1 UNTIL       
                 WS-ROW-ITERATOR > 1 OR  SO-NOT-CONTINUE-WITH-POSITION  
                  EVALUATE TRUE                                         
                    WHEN ( WS-COLUMN-ITERATOR = 1 OR -1 ) AND           
                          WS-ROW-ITERATOR = 0                           
                    WHEN ( WS-ROW-ITERATOR = 1 OR -1 ) AND              
                          WS-COLUMN-ITERATOR = 0                        
                                                                        
                        MOVE WS-ROW-ITERATOR   TO WS-ROW-ITERATOR-FORMAT
                        MOVE WS-COLUMN-ITERATOR TO                      
                                                  WS-COL-ITERATOR-FORMAT
                        DISPLAY '2175 WS-ROW-ITERATOR: '                
                           WS-ROW-ITERATOR-FORMAT                       
                        DISPLAY '2175 WS-COLUMN-ITERATOR: '             
                           WS-COL-ITERATOR-FORMAT                       
                        ADD WS-COLUMN-ITERATOR TO WS-COLUMN-CHECK-POS   
                        ADD WS-ROW-ITERATOR    TO WS-ROW-CHECK-POS      
                        DISPLAY 'WS COLUMN-CHECK-POS: '                 
                                  WS-COLUMN-CHECK-POS                   
                        DISPLAY 'WS ROW   -CHECK-POS: '                 
                                  WS-ROW-CHECK-POS                      
                        DISPLAY 'WS-COLUMN-POSITION  : '                
                               WS-COLUMN-POSITION                       
                        DISPLAY 'WS-ROW-POSITION  : '                   
                               WS-ROW-POSITION                          
                        PERFORM 2180-CHECK-GIVEN-FIELD                  
                    WHEN OTHER                                          
                        CONTINUE                                        
                  END-EVALUATE                                          
                                                                        
                  MOVE  WS-COLUMN-POSITION     TO WS-COLUMN-CHECK-POS   
                  MOVE  WS-ROW-POSITION        TO WS-ROW-CHECK-POS      
               END-PERFORM                                              
           END-PERFORM                                                  
                                                                        
           .                                                            
      ******************************************************************
      *                   2180-CHECK-GIVEN-FIELD                        
      * PARAGRAPH WILL CHECK IF NEIGHBOURS OF A FIELD WE ARE CHECKING   
      * RIGHT NOW IS A 'Z' OR NOT ( 'Z' REPRESENTS A FIELD THAT IS      
      * A UNIT OF THE SHIP THAT WAS COMPLETLY DESTROYED,  ( ALL         
      * FIELDS WERE HIT)                                                
      ******************************************************************
       2180-CHECK-GIVEN-FIELD.                                          
           DISPLAY '2180 PERFORMED '                                    
           IF WS-ROW-CHECK-POS >= 1 AND WS-ROW-CHECK-POS <= 10 AND      
            WS-COLUMN-CHECK-POS >= 1 AND WS-COLUMN-CHECK-POS <= 10 THEN 
                                                                        
             DISPLAY '2180 IN IF STATEMENT  '                           
             MOVE WS-PROGRAM-ARRAY(WS-ROW-CHECK-POS)(                   
              WS-COLUMN-CHECK-POS:1)  TO SW-WHAT-TYPE-OF-FIELD          
             IF SO-DESTROYED-SHIP-FIELD  THEN                 
               DISPLAY '2180 NEIGHBOUR DESTROYED '                      
               SET  SO-NOT-CONTINUE-WITH-POSITION TO TRUE               
             END-IF                                                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2181-WAIT-FOR-HALF-SECOND                     
      * PROGRAM WILL FORCE CICS TO WAIT FOR 1 SECOND BEFORE             
      * TAKING ANY ACTION                                               
      ******************************************************************
       2181-WAIT-FOR-HALF-SECOND.                                       
           DISPLAY '2182 WAIT FOR 1 SECOND'                             
           EXEC CICS                                                    
           DELAY FOR MILLISECS(500)                                     
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2200-CHECK-EIBRESP                           
      ******************************************************************
       2200-CHECK-EIBRESP.                                              
           DISPLAY '2200-PROCESS '                                      
           EVALUATE EIBRESP                                             
           WHEN DFHRESP(NORMAL)                                         
              DISPLAY 'NORMAL'                                          
           WHEN DFHRESP(MAPFAIL)                                        
              CONTINUE                                                  
           WHEN OTHER                                                   
              DISPLAY 'OTHER '                                          
              MOVE EIBRESP TO  WS-EIBRESP-FORMAT                        
              STRING 'OTHER ERROR ', WS-EIBRESP-FORMAT                  
              DELIMITED BY SIZE                                         
              INTO MSGO                                                 
              END-STRING                                                
              SET SO-DISPLAY-ERROR TO TRUE                              
              PERFORM 2100-SEND-THE-MAP                                 
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   2201-SEND-THE-FIRST-MULTI-MAP                 
      ******************************************************************
       2201-SEND-THE-FIRST-MULTI-MAP.                                   
           DISPLAY '2201 SEND THE FIRST MULTI MAP '                     
           EXEC CICS                                                    
             SEND MAP('MP0236') MAPSET('MP0236')                        
             FROM(MP0236O)                                              
             ERASE                                                      
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2202-VALIDATE-USER-NICK                      
      * IF NICK IS EQUAL TO 'XXXXXX' THEN IT IS INVALID                 
      * THIS SYMBOL WILL REPRESENT THAT THE PLAYER WAS DISCONNECTED     
      * SO IT CANNOT BY A NAME OF THE PLAYER                            
      *                                                                 
      * NICK EQUAL TO 'AAAAAA' IS ALSO INVALID THIS SYMBOL INDICATES    
      * THAT ENEMY WON THE GAME                                         
      *                                                                 
      * NICK EQUAL TO 'BBBBBB' IS ALSO INVALID THIS SYMBOL INDICATES    
      * THAT ENEMY IS INACTIVE                                          
      *                                                                 
      *                                                                 
      * NICK FIELD ON THE MAP IS FILLED WITH UNDERSCORES '_'            
      * SO WE WILL GET VALUE LIKE :  USER NICK____                      
      * TO DELETE THOSE UNDERSCORES WE WILL USER INSPECT STATEMENT      
      ******************************************************************
       2202-VALIDATE-USER-NICK.                                         
           PERFORM 2203-RECEIVE-FIRST-MULTI-MAP                         
           INSPECT WS-USER-NICK REPLACING ALL '_' BY ' '                
           IF WS-USER-NICK = SPACE         OR                           
              CT-DISCONNECTED-SYMBOL-TEXT  OR 
              CT-USER-WIN-SYMBOL-TEXT      OR                           
              CT-ENEMY-INACTIVE-SYMBOL     THEN                         
                                                                        
              SET SO-INVALID-NICK          TO TRUE                      
           ELSE                                                         
              SET SO-VALID-NICK            TO TRUE                      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                       2203-RECEIVE-FIRST-MULTI-MAP.             
      ******************************************************************
       2203-RECEIVE-FIRST-MULTI-MAP.                                    
           EXEC CICS                                                    
             RECEIVE MAP('MP0236') MAPSET('MP0236')                     
             INTO(MP0236I)                                              
             NOHANDLE                                                   
           END-EXEC                                                     
           IF EIBRESP = DFHRESP(MAPFAIL)                                
              MOVE 'PLEASE PROVIDE THE NICK ' TO MSG3O                  
              SET SO-INVALID-NICK   TO TRUE                             
           ELSE                                                         
              MOVE NICKI TO WS-USER-NICK                                
              PERFORM 2200-CHECK-EIBRESP                                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2205-CHECK-NEIGHBOURS                         
      * THIS PARAGRAPH WILL BE PERFORMED 2 TIMES AND THIS LOOP IN THIS  
      * PARAGRAPH WILL ALSO BE PERFORMED 2 TIMES                        
      *                                                                 
      * THANKS TO THAT LOGIC THIS PARAGRAPH WILL CHECK ALL NEIGHBOURS   
      *  OF THE "MAIN" FIELD                                            
      *                                                                 
      * ADDITIONAL LOGIC IN THAT FIELD IS:                              
      *                                                                 
      * IF PREVIOS CHECK WAS CORRECT (SO OPPOSITE IS VALID) BUT  
      * OUR FIELD THAT WE ARE CHECKING RIGHT NOW IS A 'X' THEN          
      * WE WILL CHECK THIS DIRECTION ( THAT IS BECAUSE WHEN WE HAVE     
      * AN 'X' THIS IS MOST IMPORANT SITUATION ) SO IT CAN BE           
      * PERFORMED EVEN IF EARILER THERE WAS A OTHER VALID DIRECTION     
      ******************************************************************
       2205-CHECK-NEIGHBOURS.                                           
           PERFORM VARYING WS-ITER1 FROM -1 BY 2 UNTIL WS-ITER1 > 1     
             MOVE WS-ROW-POSITION     TO WS-TEMP-ROW                    
             MOVE WS-COLUMN-POSITION  TO WS-TEMP-COLUMN                 
                                                                        
             IF SO-HORIZONTAL-CHECK THEN                                
      * HORIZONTAL NEIGHBOUR CHECK                                      
               ADD WS-ITER1 TO WS-TEMP-COLUMN                           
               IF WS-TEMP-COLUMN >= 1 AND WS-TEMP-COLUMN <= 10 THEN     
                                                                        
                 IF ( SO-OPPOSITE-IS-VALID AND                          
                  WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) = 'X')
                         OR SO-OPPOSITE-IS-INVALID                      
                                                                        
                     PERFORM 2188-DECIDE-WHERE-TO-SHOOT                 
                 END-IF                                                 
               END-IF                                                   
             ELSE                                                       
      * VERTICAL NEIGBHOURS CHECK                                       
               ADD WS-ITER1 TO WS-TEMP-ROW                              
               IF WS-TEMP-ROW >= 1 AND WS-TEMP-ROW <= 10                
                                                                        
                 IF ( SO-OPPOSITE-IS-VALID AND                          
                  WS-PROGRAM-ARRAY(WS-TEMP-ROW)(WS-TEMP-COLUMN:1) = 'X')
                         OR SO-OPPOSITE-IS-INVALID                      
                  PERFORM 2188-DECIDE-WHERE-TO-SHOOT                    
                 END-IF                                                 
                END-IF                                                  
             END-IF                                                     
                                                                        
           END-PERFORM      
           .                                                            
      ******************************************************************
      *                   2206-DELAY-FOR-5-SECONDS                      
      ******************************************************************
       2206-DELAY-FOR-5-SECONDS.                                        
           DISPLAY '2206 WAIT FOR 5 SECOND'                             
           EXEC CICS                                                    
           DELAY FOR MILLISECS(5000)                                    
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                    2210-GET-NICK-LENGTH                         
      ******************************************************************
       2210-GET-NICK-LENGTH.                                            
           MOVE SPACE                          TO WS-STRING-TEMP        
           MOVE 0                              TO WS-SPACE-COUNTER      
           MOVE 0                              TO WS-USER-NICK-LEN      
                                                                        
           MOVE FUNCTION REVERSE(WS-USER-NICK) TO WS-STRING-TEMP        
           INSPECT WS-STRING-TEMP TALLYING WS-SPACE-COUNTER             
                          FOR LEADING SPACES                            
           COMPUTE WS-USER-NICK-LEN = LENGTH OF WS-USER-NICK -          
                                           WS-SPACE-COUNTER             
           DISPLAY '2210 WS-SER-NICK: '   WS-USER-NICK                  
           DISPLAY '2210 WS-USER-NICK LENGTH: ' WS-USER-NICK-LEN        
           .                                                            
      ******************************************************************
      *                    2211-MOVE-BOARD-TO-ARRAY                     
      ******************************************************************
       2211-MOVE-BOARD-TO-ARRAY.                                        
           DISPLAY '2211 TUTAJ POWINNY BYC VALID ENEMY '                
           UNSTRING T02-GAME-BOARD-TEXT                                 
           INTO WS-PROGRAM-ARRAY(1),                                    
                WS-PROGRAM-ARRAY(2),                                    
                WS-PROGRAM-ARRAY(3),          
                WS-PROGRAM-ARRAY(4),                                    
                WS-PROGRAM-ARRAY(5),                                    
                WS-PROGRAM-ARRAY(6),                                    
                WS-PROGRAM-ARRAY(7),                                    
                WS-PROGRAM-ARRAY(8),                                    
                WS-PROGRAM-ARRAY(9),                                    
                WS-PROGRAM-ARRAY(10)                                    
                                                                        
           END-UNSTRING                                                 
           DISPLAY '2211 CALY BOARD: '                                  
           DISPLAY 'WS-SCREE(1) : ' WS-PROGRAM-ARRAY(1)                 
           DISPLAY 'WS-SCREE(2) : ' WS-PROGRAM-ARRAY(2)                 
           DISPLAY 'WS-SCREE(3) : ' WS-PROGRAM-ARRAY(3)                 
           DISPLAY 'WS-SCREE(4) : ' WS-PROGRAM-ARRAY(4)                 
           DISPLAY 'WS-SCREE(5) : ' WS-PROGRAM-ARRAY(5)                 
           DISPLAY 'WS-SCREE(6) : ' WS-PROGRAM-ARRAY(6)                 
           DISPLAY 'WS-SCREE(7) : ' WS-PROGRAM-ARRAY(7)                 
           DISPLAY 'WS-SCREE(8) : ' WS-PROGRAM-ARRAY(8)                 
           DISPLAY 'WS-SCREE(9) : ' WS-PROGRAM-ARRAY(9)                 
           DISPLAY 'WS-SCREE(10) : 'WS-PROGRAM-ARRAY(10)                
           .                                                            
      ******************************************************************
      *                 2214-MOVE-BOARDS-TO-SCREEN                      
      * PARAGRAPH WILL BE USED IN ORDER TO GET BOARD DATA               
      * FROM THE DATABASE INTO THE SCREEN VARIABLES                     
      *                                                                 
      * DEPNDING ON THE FLAGS PARAGRAPH CAN PREPARE                     
      * USER'S BOARD OR ENEMY'S BOARD                                   
      ******************************************************************
       2214-MOVE-BOARDS-TO-SCREEN.                                      
           IF SO-GET-THIS-USER-BOARD  THEN                              
              UNSTRING T02-GAME-BOARD-TEXT                              
              INTO                                                      
                       POLEUO(1),                                       
                       POLEUO(2),                                       
                       POLEUO(3),    
                       POLEUO(4),                                       
                       POLEUO(5),                                       
                       POLEUO(6),                                       
                       POLEUO(7),                                       
                       POLEUO(8),                                       
                       POLEUO(9),                                       
                       POLEUO(10)                                       
              END-UNSTRING                                              
              DISPLAY '2214 USER BOARD WHOLE: '                         
              DISPLAY   POLEUO(1)                                       
              DISPLAY   POLEUO(2)                                       
              DISPLAY   POLEUO(3)                                       
              DISPLAY   POLEUO(4)                                       
              DISPLAY   POLEUO(5)                                       
              DISPLAY   POLEUO(6)                                       
              DISPLAY   POLEUO(7)                                       
              DISPLAY   POLEUO(8)                                       
              DISPLAY   POLEUO(9)                                       
              DISPLAY   POLEUO(10)                                      
           ELSE                                                         
      * IN THE DATABASE ENEMY'S BOARD IS SAVED AS OURS                  
      * SO THERE IS AN INFORMATION ABOUT WHERE ARE 'S' - SHIPS          
      * WE DON'T WANT TO SHARE THAT KNOWLEDGE SO WE WILL                
      * REPLACE ALL 'S' SYMBOLS WITH THE SPACE                          
              INSPECT T02-GAME-BOARD-TEXT REPLACING ALL 'S' BY ' '      
              UNSTRING T02-GAME-BOARD-TEXT                              
              INTO                                                      
                       POLEKO(1),                                       
                       POLEKO(2),                                       
                       POLEKO(3),                                       
                       POLEKO(4),                                       
                       POLEKO(5),                                       
                       POLEKO(6),                                       
                       POLEKO(7),                                       
                       POLEKO(8),                                       
                       POLEKO(9),    
                       POLEKO(10)                                       
              END-UNSTRING                                              
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2215-MULTIPLAYER-SHOOTS                      
      * PARAGRAPH WILL BE CALLED WHEN ONE OUT OF 2 USERS WILL           
      * SHOOT AT THE OTHER                                              
      *                                                                 
      * THIS PARAGRAPH IS RESPONSIBLE FOR DISPLAYING BOTH BOARDS        
      * ALONG WITH CHEKING IF GAME ENDED                                
      * AND OF COURSE CHECKING IF USER HIT SOMETHING OR MISSED          
      ******************************************************************
       2215-MULTIPLAYER-SHOOTS.                                         
                                                                        
           PERFORM 2003-RECEIVE-MAP-FROM-USER                           
           SET     SO-GET-ENEMY-BOARD  TO TRUE                          
           PERFORM 7011-GET-THE-GAME-BOARD                              
           PERFORM 2211-MOVE-BOARD-TO-ARRAY                             
                                                                        
           PERFORM 2116-USER-SHOOTS                                     
                                                                        
           PERFORM 7015-SAVE-THE-BOARD-TO-DB2                           
           PERFORM 2216-GET-USER-BOARD-TO-SCREEN                        
                                                                        
           PERFORM 2220-CHECK-IF-END-OF-GAME                            
           PERFORM 2100-SEND-THE-MAP                                    
           IF NOT SO-GAME-SHOULD-END THEN                               
             PERFORM 2181-WAIT-FOR-HALF-SECOND                          
             PERFORM 2170-SEND-SINGLE-MAP-CURSOR                        
                                                                        
      * IF USER MISSED A SHOT TURN IS SWITCH TO THE ENEMY               
      * SO WE WILL WAIT FOR OUR TURN                                    
             IF SO-TURN-CHANGES THEN                                    
               SET SO-WAIT-FOR-OPPONENT TO TRUE                         
             END-IF     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *               2216-GET-USER-BOARD-TO-SCREEN.                    
      ******************************************************************
       2216-GET-USER-BOARD-TO-SCREEN.                                   
           SET SO-GET-THIS-USER-BOARD TO TRUE                           
           PERFORM 7011-GET-THE-GAME-BOARD                              
                                                                        
           IF T02-GAME-BOARD-TEXT = SPACE OR LOW-VALUES                 
              AND NOT SO-GAME-SHOULD-END                                
            THEN                                                        
                                                                        
                PERFORM 2130-INITIALIZE-MAP                             
           ELSE                                                         
                                                                        
               PERFORM 2105-PROTECT-USER-FIELDS                         
           END-IF                                                       
           PERFORM 2214-MOVE-BOARDS-TO-SCREEN                           
           .                                                            
      ******************************************************************
      *               2217-MULTIP-SHIP-VALIDATION                       
      ******************************************************************
       2217-MULTIP-SHIP-VALIDATION.                                     
           DISPLAY 'VALIDACJA STATKOW '                                 
           PERFORM 2241-PREPARE-SHIPS-ARRAY                             
           PERFORM 2003-RECEIVE-MAP-FROM-USER                           
           PERFORM 2033-VALIDATE-USER-SHIPS                             
           IF SO-VALID-USER-SHIPS THEN                                  
                DISPLAY '2002 SO-VALID-SHIPS '                          
                                                                        
      * IF THIS USER PROVIDED VALID SHIPS THEN WE HAVE TO SAVE          
      * THEM INTO THE DATABASE                                          
      * WE WILL ALSO BLOCK THE FIELD (USER WILL NOT BE ABLE TO MODIFY   
      * HIS SHIPS) AND ALSO WE WILL SWITCH THE TURNS                    
      * (ENEMY WILL HAVE NEXT TURN)    
                 PERFORM 7014-SAVE-THE-BOARD-TO-DB2                     
                 DISPLAY '"ENEMY TURN" SHOULD BE DISPLAYED'             
                 MOVE 'ENEMY TURN' TO MSGO                              
                 PERFORM 2100-SEND-THE-MAP                              
                 PERFORM 2181-WAIT-FOR-HALF-SECOND                      
                 SET SO-WAIT-FOR-OPPONENT TO TRUE                       
           ELSE                                                         
      * IF SHIPS ARE INVALID THEN USER'S BOARD WILL BE INITIZATED       
      * AND USER WILL HAVE TO PROVIDE SHIPS ONCE AGAIN                  
              DISPLAY '2217 SO-INVALID-SHIPS '                          
                                                                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *               2218-PREP-DATA-WAIT-FOR-ENEMY                     
      ******************************************************************
       2218-PREP-DATA-WAIT-FOR-ENEMY.                                   
           PERFORM 2303-PREP-DATA-BEFORE-WAITING                        
                                                                        
           MOVE 'ENEMY TURN ' TO MSG3O                                  
           PERFORM 2100-SEND-THE-MAP                                    
           PERFORM 2181-WAIT-FOR-HALF-SECOND                            
           SET SO-WAIT-FOR-OPPONENT TO TRUE                             
      *    PERFORM 2222-CHECK-UNTIL-OUR-TURN                            
           .                                                            
      ******************************************************************
      *                2219-CHECK-NICK-FIND-OPPONENT                    
      * PARAGRAPH WILL VALIDATE IF USER PROVIDED VALID NICK             
      * IF SO IT WILL INSERT THIS NICK TO THE DATABASE                  
      * AND PROCESS OF FINDING AN ENEMY WILL START                      
      ******************************************************************
       2219-CHECK-NICK-FIND-OPPONENT.                                   
           DISPLAY '2219-CHECK-NICK-FIND-OPPONENT PERFORMED'            
           DISPLAY 'SODMODE-MULTIPLAYER IS TRUE '                       
           PERFORM 2202-VALIDATE-USER-NICK                              
           IF   SO-VALID-NICK THEN    
                                                                        
              PERFORM 7001-INSERT-NICK-INTO-DATABSE                     
              IF SO-USER-PROVIDED-NICK THEN                             
      * IF USER PROVIDED VALID NICK THEN WE SHOULD CHECK                
      * IF THERE IS ANY OLD DATA IN THE DATABAES                        
      * IF SO WE WILL DELETE THAT                                       
                DISPLAY 'SO-USER-PROVIDED NICK '                        
                PERFORM 7017-DELETE-THIS-NICK-DATA                      
                PERFORM 7002-WAIT-UNTIL-PLAYER-FOUND                    
              ELSE                                                      
                DISPLAY 'SO- DID NOT PROVIDE A NICK '                   
                PERFORM 2201-SEND-THE-FIRST-MULTI-MAP                   
              END-IF                                                    
           ELSE                                                         
              PERFORM 2201-SEND-THE-FIRST-MULTI-MAP                     
           END-IF                                                       
           .                                                            
      ******************************************************************
      *               2220-CHECK-IF-END-OF-GAME                         
      ******************************************************************
       2220-CHECK-IF-END-OF-GAME.                                       
           PERFORM 2221-GET-BOTH-BOARDS                                 
           PERFORM 2139-IF-GAME-ENDED-OR-NOT                            
           .                                                            
      ******************************************************************
      *                   2221-GET-BOTH-BOARDS                          
      ******************************************************************
       2221-GET-BOTH-BOARDS.                                            
           SET SO-GET-THIS-USER-BOARD   TO TRUE                         
           PERFORM 7011-GET-THE-GAME-BOARD                              
            UNSTRING T02-GAME-BOARD-TEXT                                
             INTO                                                       
                     WS-USER-BOARD-LINE(1),                             
                     WS-USER-BOARD-LINE(2),                             
                     WS-USER-BOARD-LINE(3),                             
                     WS-USER-BOARD-LINE(4),  
                     WS-USER-BOARD-LINE(5),                             
                     WS-USER-BOARD-LINE(6),                             
                     WS-USER-BOARD-LINE(7),                             
                     WS-USER-BOARD-LINE(8),                             
                     WS-USER-BOARD-LINE(9),                             
                     WS-USER-BOARD-LINE(10)                             
             END-UNSTRING                                               
           SET SO-GET-ENEMY-BOARD    TO TRUE                            
           PERFORM 7011-GET-THE-GAME-BOARD                              
            UNSTRING T02-GAME-BOARD-TEXT                                
             INTO                                                       
                     WS-COMPUTER-BOARD-LINE(1),                         
                     WS-COMPUTER-BOARD-LINE(2),                         
                     WS-COMPUTER-BOARD-LINE(3),                         
                     WS-COMPUTER-BOARD-LINE(4),                         
                     WS-COMPUTER-BOARD-LINE(5),                         
                     WS-COMPUTER-BOARD-LINE(6),                         
                     WS-COMPUTER-BOARD-LINE(7),                         
                     WS-COMPUTER-BOARD-LINE(8),                         
                     WS-COMPUTER-BOARD-LINE(9),                         
                     WS-COMPUTER-BOARD-LINE(10)                         
             END-UNSTRING                                               
           .                                                            
      ******************************************************************
      *                  2222-CHECK-UNTIL-OUR-TURN                      
      ******************************************************************
       2222-CHECK-UNTIL-OUR-TURN.                                       
           SET SO-ENEMY-TOUR       TO TRUE                              
           MOVE 0                  TO WS-HOW-MANY-LOOPS                 
      * WS-LAST-EIBCPOSN NORMALLY WILL STORE NUMBER OF FIELD WERE WAS   
      * THE LAST SHOT OF THE USER BUT WHEN THERE WAS NO                 
      * SHOT, WE WILL PUT THERE NUMBER OF THE FIRST FIELD ON THE        
      * ENEMY'S BOARD                                                   
           IF WS-LAST-EIBCPOSN = 0 THEN                                 
             MOVE CT-FIRST-FIELD-ON-ENEMY-BOARD  TO WS-LAST-EIBCPOSN    
           END-IF                      
           PERFORM UNTIL SO-OUR-TURN OR WS-HOW-MANY-LOOPS > 20          
               OR  SO-ENEMY-IS-DISCONECTED                              
              ADD 1 TO WS-HOW-MANY-LOOPS                                
              PERFORM 7010-CHECK-WHOSE-TURN                             
              IF NOT SO-OUR-TURN AND SO-ENEMY-IS-CONNECTED              
              AND NOT  SO-ENEMY-WON-THE-GAME                            
              AND NOT SO-PLAYER-WAS-INACTIVE THEN                       
                PERFORM 2206-DELAY-FOR-5-SECONDS                        
              END-IF                                                    
           END-PERFORM                                                  
                                                                        
           PERFORM 2224-ERROR-IF-ENEMY-INACTIVE                         
           PERFORM 2225-SEND-MESSG-IF-OUR-TURN                          
           PERFORM 2226-ERROR-IF-ENEMY-DISCON                           
           PERFORM 2227-SEND-MESSG-IF-ENEMY-WON                         
           PERFORM 2228-SEND-MESSG-IF-INACTIVE                          
           .                                                            
      ******************************************************************
      *                  2223-CICS-SYNCPOINT                            
      * PARAGRAPH LOGIC IS TO ISSUE CICS'S SYNCPOINT COMMAND            
      * IT WILL WORK AS "COMMIT" IN THE BATCH                           
      ******************************************************************
       2223-CICS-SYNCPOINT.                                             
           EXEC CICS                                                    
             SYNCPOINT                                                  
           END-EXEC                                                     
           PERFORM 2200-CHECK-EIBRESP                                   
           .                                                            
      ******************************************************************
      *                  2224-ERROR-IF-ENEMY-INACTIVE                   
      ******************************************************************
       2224-ERROR-IF-ENEMY-INACTIVE.                                    
      * IF STATEMENT BELOW WILL BE TRUE WHEN THIS PLAYER WAITED         
      * LONEGER THAN 100 SECONDS FOR ITS TURN                           
      *                                                                 
           IF WS-HOW-MANY-LOOPS > 20 THEN  
              DISPLAY 'GAME ENEDED DUE TO INACTIVITY '                  
              MOVE 'GAME ENDED DUE TO INACTIVE ENEMY PLAYER ' TO MSGO   
              SET SO-GAME-SHOULD-END          TO TRUE                   
              SET SO-TERMINATION-WITHOUT-MESS TO TRUE                   
              PERFORM 2170-SEND-SINGLE-MAP-CURSOR                       
                                                                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2225-SEND-MESSG-IF-OUR-TURN                   
      ******************************************************************
       2225-SEND-MESSG-IF-OUR-TURN.                                     
            IF SO-OUR-TURN AND NOT  SO-ENEMY-IS-DISCONECTED THEN        
      * PARAGRAPH ABOVE WILL CHECK IF WE GOT SHIPS OR NOT               
               PERFORM 7011-GET-THE-GAME-BOARD                          
               IF SO-WE-DONT-HAVE-SHIPS THEN                            
                 MOVE 'PLACE YOUR SHIPS AT LEFT BY LETTER "S" '         
                 TO MSGO                                                
               ELSE                                                     
                 MOVE 'YOUR TURN! ' TO MSGO                             
                 PERFORM 2216-GET-USER-BOARD-TO-SCREEN                  
               END-IF                                                   
                                                                        
               PERFORM 2170-SEND-SINGLE-MAP-CURSOR                      
                                                                        
            END-IF                                                      
            .                                                           
      ******************************************************************
      *               2226-ERROR-IF-ENEMY-DISCON                        
      ******************************************************************
       2226-ERROR-IF-ENEMY-DISCON.                                      
           DISPLAY '2226 PERFORMED  '                                   
           IF   SO-ENEMY-IS-DISCONECTED THEN                            
             DISPLAY '2226 ENEMY IS DISCONNECTED '                      
             SET SO-GAME-SHOULD-END              TO TRUE                
             PERFORM 2216-GET-USER-BOARD-TO-SCREEN         
                                                                        
             MOVE ' YOUR ENEMY IS DISCONNECTED ' TO MSGO                
             PERFORM 2170-SEND-SINGLE-MAP-CURSOR                        
             PERFORM 2170-SEND-SINGLE-MAP-CURSOR                        
                                                                        
             SET  SO-TERMINATION-WITHOUT-MESS    TO TRUE                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2227-SEND-MESSG-IF-ENEMY-WON                   
      ******************************************************************
       2227-SEND-MESSG-IF-ENEMY-WON.                                    
           IF SO-ENEMY-WON-THE-GAME THEN                                
              MOVE ' YOUR WERE DEFEATED         ' TO MSGO               
                                                                        
              PERFORM 2216-GET-USER-BOARD-TO-SCREEN                     
                                                                        
              PERFORM 2170-SEND-SINGLE-MAP-CURSOR                       
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    228-SEND-MESSG-IF-INACTIVE.                  
      ******************************************************************
       2228-SEND-MESSG-IF-INACTIVE.                                     
           IF SO-PLAYER-WAS-INACTIVE THEN                               
              MOVE ' GAME ENDED DUE TO YOUR INACTIVITY  ' TO MSGO       
                                                                        
              PERFORM 2216-GET-USER-BOARD-TO-SCREEN                     
                                                                        
              PERFORM 2170-SEND-SINGLE-MAP-CURSOR                       
              PERFORM 7020-DELETE-THIS-GAME-DATA                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2230-VALIDATE-SHIPS-OR-SHOOT                   
      * PARAGRAPH HAS TWO FUNCTIONALITIES       
      * 1. CHEKCING IF USER PROVIDED VALID SHIPS                        
      * 2. SHOOTING AT THE ENEMY BOARD                                  
      *                                                                 
      * TO DETERMINE WHAT WE ARE GONNA DO WE HAVE TO ISSUE SQL          
      * QUERY TO CHECK IF OUR GAME BOARD IS EMPTY OR NOT                
      * IF IT IS EMPTY AND WE ARE HERE IT MEANS WE SHOULD VALIDATE      
      * THIS SHIPS                                                      
      *                                                                 
      * IF IT IS NOT EMPTY IT MEANS THAT WE SHOULD VALIDATE A SHOT      
      ******************************************************************
       2230-VALIDATE-SHIPS-OR-SHOOT.                                    
           DISPLAY '2230-VALIDATE-SHIPS-OR-SHOOT PERFORMED'             
           SET SO-GET-THIS-USER-BOARD     TO TRUE                       
      * THIS FLAG WILL DETERMINE IF WE SHOULD WAIT FOR THE OPPONENT     
      * MOVE HERE OR IF WE SHOULD CONTINUE                              
           SET SO-DONT-WAIT-FOR-OPPONENT  TO TRUE                       
           PERFORM 7011-GET-THE-GAME-BOARD                              
                                                                        
           IF SO-WE-DONT-HAVE-SHIPS THEN                                
             PERFORM 2217-MULTIP-SHIP-VALIDATION                        
           ELSE                                                         
      * IF THIS USER ALREADY HAS THE SHIPS AND HE PRESSED ENTER         
      * IT MEANS THAT HE CHOOSE THE POSITION AND PRESSED ENTER          
      * (WE WILL HAVE TO GET THIS POSITION AND VALIDATE IF HE           
      * HIT SOMETHING OR HE JUST MISSED )                               
             PERFORM 2215-MULTIPLAYER-SHOOTS                            
           END-IF                                                       
                                                                        
                                                                        
                                                                        
                                                                        
           .                                                            
      ******************************************************************
      *                  2231-END-THE-GAME-OR-WAIT                      
      * PARAGRAPH WILL DISPLAY MESSAGE INDICATING THAT ENEMY IS         
      * DISOCNNECTED OR WILL WAIT AS LONG AS IT WILL BE OUR TURN   
      * OR AS LONG AS IT IS POSSIBLE ( IF WE WAIT TO LONG PROPER        
      * ERROR MESSAGE WILL BE DISPLAYED )                               
      ******************************************************************
       2231-END-THE-GAME-OR-WAIT.                                       
           DISPLAY '2231-END-THE-GAME-OR-WAIT PERFORMED'                
           IF SO-ENEMY-IS-DISCONECTED THEN                              
             MOVE 'YOU WIN (ENEMY DISCONECTED) ' TO MSGO                
                                                                        
             SET SO-GAME-SHOULD-END  TO TRUE                            
           ELSE                                                         
             PERFORM 2218-PREP-DATA-WAIT-FOR-ENEMY                      
           END-IF                                                       
           .                                                            
      ******************************************************************
      *              2232-SEND-CORRECT-USER-MAP                         
      ******************************************************************
       2232-SEND-CORRECT-USER-MAP.                                      
      * IF STAEMENT BELOW WILL BE TRUE IF USER PROVIDED VALID           
      * SHOT POSITION.                                                  
      * IF IT IS TRUE THEN WE WILL SEND MAP WITH CURSOR OPTION          
      * AND THANKS TO THAT USER'S CURSOR POSITION WILL BE               
      * SAVED ON THE MAP                                                
           DISPLAY '2232-SEND-CORRECT-USER-MAP    PERFORMED '           
           IF SO-USER-SHOT-POSITION-VALID THEN                          
                 DISPLAY '2232 SO-USER-SHOT-POSITOIN WAS VALID '        
                 PERFORM 2170-SEND-SINGLE-MAP-CURSOR                    
           ELSE                                                         
                 DISPLAY '2232 SO NOT USER-SHOT-POSITOIN WAS VALID '    
                                                                        
                  PERFORM 2100-SEND-THE-MAP                             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2233-PROCESS-DATA-WHEN-MULTIP                   
      ******************************************************************
       2233-PROCESS-DATA-WHEN-MULTIP.       
           DISPLAY '2233-PROCESS-DATA-WHEN-MULTIP PERFORMED '           
                IF SO-OUR-ENEMY-MADE-RECORD  THEN                       
                   DISPLAY '2233 ENEMY MADE A RECORD '                  
      * WE WILL GET HERE IF USER CREATED HIS EMPTY BOARD                
      * AND RECORD FROM T03_BATTLESHIP_MAIN_TABLE IS CREATED            
      * (IT WILL CONTROL THE GAME)                                      
                                                                        
      * FIRST WE NEED TO GET INFO WHOSE GOT THE TURN RIGHT NOW          
                                                                        
                  PERFORM 7010-CHECK-WHOSE-TURN                         
                  IF SO-OUR-TURN   THEN                                 
                     DISPLAY '2233 SO IT IS OUR TURN '                  
                     PERFORM 2230-VALIDATE-SHIPS-OR-SHOOT               
                  ELSE                                                  
                     DISPLAY '2233  IT IS ENEMY TURN '                  
                     PERFORM 2231-END-THE-GAME-OR-WAIT                  
                  END-IF                                                
                ELSE                                                    
                  DISPLAY 'ENEMY DIDNT MAKE A RECORD'                   
                  DISPLAY 'WE HAVE TO VALIDATE THE NICK  '              
      * PARAGRAPH WILL CHECK IF USER PROVIDED A VALID NICK              
      * IF SO IT WILL BE SAVED INTO THE DATABASE                        
      * AND PROGRAM WILL START PROCESS OF FINDING THE OPPONENET         
                  PERFORM 2219-CHECK-NICK-FIND-OPPONENT                 
                END-IF                                                  
           .                                                            
      ******************************************************************
      *                  2234-PROCESS-DATA-WHEN-SINGLP                  
      ******************************************************************
       2234-PROCESS-DATA-WHEN-SINGLP.                                   
           DISPLAY '2234-VALIDATE-SINGLEP-INPUT PERFORMED '             
                                                                        
      * THIS IF STATEMENT WONT EXECUTE A RECEIVE COMMAND WHEN           
      * THIS IS THE COMPUTER TURN                                       
      * THAT 'IF' IS NEEDED BECAUSE IF THIS COMMAND WILL BE EXECUTED    
      * HERE THEN PROGRAM WILL BE BLOCKED WITHOUT ANY REASON 
      * SO RECEVIE STATEMENT WILL BE EXECUTED ONLY WHEN IT IS NOT       
      * A COMPUTER MOVE                                                 
           IF SO-TURN-CHANGED-TO-COMPUTER                               
                                                                        
             DISPLAY 'SO TURN CHANGED TO COMPUTER NO ACTION '           
           ELSE                                                         
             DISPLAY 'SO NOT TURN CHANGED TO COMPUTER '                 
             PERFORM 2003-RECEIVE-MAP-FROM-USER                         
           END-IF                                                       
                                                                        
           PERFORM 2032-PROCESS-SINGLE-INPUT                            
           PERFORM 2232-SEND-CORRECT-USER-MAP                           
                                                                        
           .                                                            
      ******************************************************************
      *                2235-PLACE-VERTICAL-SHIP                         
      * PARAGRAPH IS CALLED IN ORDER TO INSERT VERTICAL SHIP            
      * ON THE BOARD                                                    
      *                                                                 
      * WE ALREADY KNOW THE POSITION WHERE SHOULD BE A FIRST FIELD      
      * OF THE SHIP                                                     
      * SO WE WILL PLACE 'S' THERE AND ON ALL OTHER FIELDS WHERE        
      * THIS SHIP SHOULD BE                                             
      *                                                                 
      * PARAGRAPH WILL ALSO MAKE VALIDATION IF SHIP CAN BE PLACED       
      * IN EVERY POSITION WHERE PROGRAMS TRIES TO PLACE IT              
      * (WE WILL VALIDATE NEIGHBORS OF THE FIELDS)                      
      * IF EVERYTHING GOES GOOD THEN AFTER THIS PARAGRAPH               
      * WHOLE SHIP WILL BE PLACED ON THE TEMORARY BOARD                 
      * AND THIS TEMPORARY BOARD WILL BE SAVED AS MAIN ONE              
      *                                                                 
      * IF SOME PARAT OF THIS PROCES WILL GO WRONG THEN                 
      * TEMPRORARY ARRAY WILL BE RESETED AND PROCESS WILL START         
      * FROM THE SCRATCH ONCE AGAIN                                     
      ******************************************************************
       2235-PLACE-VERTICAL-SHIP.   
           DISPLAY '2014 VERTICAL PLACE '                               
           MOVE 0 TO WS-ITER2                                           
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3          
              > WS-FIELD-NUMBER OR SO-SHIP-POSITION-INVALID             
                                                                        
              MOVE WS-ROW-POSITION TO WS-ROW-POSITION-TEMP              
              ADD WS-ITER2 TO WS-ROW-POSITION-TEMP                      
              MOVE WS-ROW-POSITION-TEMP TO WS-ROW-VALIDATION            
              MOVE WS-COLUMN-POSITION TO WS-COLUMN-VALIDATION           
                                                                        
              PERFORM 2016-VALIDATE-NEIGHBORS                           
              IF SO-SHIP-POSITION-VALID THEN                            
                MOVE 'S' TO                                             
                WS-SCREEN-LINE2(WS-ROW-POSITION-TEMP)(                  
                WS-COLUMN-POSITION:1)                                   
                DISPLAY 'ALAMAKOTASTATEKPOLOZONY'                       
                DISPLAY 'PLACE AT ROW: ' WS-ROW-POSITION-TEMP           
                 'COLUMN: '  WS-COLUMN-POSITION                         
                                                                        
                IF WS-ITER2 > 10 THEN                                   
                   DISPLAY 'WS-ITER2PRZEBIL10'                          
                END-IF                                                  
                ADD 1 TO WS-ITER2                                       
              END-IF                                                    
           END-PERFORM                                                  
           IF SO-SHIP-POSITION-VALID THEN                               
             PERFORM 2030-SAVE-THE-BOARD                                
             SET SO-SHIP-WAS-PLACED TO TRUE                             
             ADD 1 TO WS-SHIP-COUNTER                                   
           ELSE                                                         
             SET SO-SHIP-WAS-NOT-PLACED TO TRUE                         
             PERFORM 2031-INITALIZE-THE-BOARD                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2236-PLACE-HORIZONTAL-SHIP      
      * PARAGRAPH IS CALLED IN ORDER TO INSERT HORIZONTAL SHIP          
      * ON THE BOARD                                                    
      *                                                                 
      * WE ALREADY KNOW THE POSITION WHERE SHOULD BE A FIRST FIELD      
      * OF THE SHIP                                                     
      * SO WE WILL PLACE 'S' THERE AND ON ALL OTHER FIELDS WHERE        
      * THIS SHIP SHOULD BE                                             
      *                                                                 
      * PARAGRAPH WILL ALSO MAKE VALIDATION IF SHIP CAN BE PLACED       
      * IN EVERY POSITION WHERE PROGRAMS TRIES TO PLACE IT              
      * (WE WILL VALIDATE NEIGHBORS OF THE FIELDS)                      
      * IF EVERYTHING GOES GOOD THEN AFTER THIS PARAGRAPH               
      * WHOLE SHIP WILL BE PLACED ON THE TEMORARY BOARD                 
      * AND THIS TEMPORARY BOARD WILL BE SAVED AS MAIN ONE              
      *                                                                 
      * IF SOME PARAT OF THIS PROCES WILL GO WRONG THEN                 
      * TEMPRORARY ARRAY WILL BE RESETED AND PROCESS WILL START         
      * FROM THE SCRATCH ONCE AGAIN                                     
      ******************************************************************
       2236-PLACE-HORIZONTAL-SHIP.                                      
           DISPLAY '2014 HORIZONTAL PLACE '                             
           MOVE 0 TO WS-ITER2                                           
                                                                        
           PERFORM VARYING WS-ITER3 FROM 1 BY 1 UNTIL WS-ITER3          
            > WS-FIELD-NUMBER   OR SO-SHIP-POSITION-INVALID             
                                                                        
              MOVE WS-COLUMN-POSITION TO WS-COLUMN-POSITION-TEMP        
                                                                        
              ADD WS-ITER2 TO WS-COLUMN-POSITION-TEMP                   
              MOVE WS-ROW-POSITION TO WS-ROW-VALIDATION                 
              MOVE WS-COLUMN-POSITION-TEMP TO WS-COLUMN-VALIDATION      
                                                                        
              PERFORM 2016-VALIDATE-NEIGHBORS                           
              IF SO-SHIP-POSITION-VALID THEN                            
                MOVE 'S' TO                                             
                WS-SCREEN-LINE2(WS-ROW-POSITION)(     
                WS-COLUMN-POSITION-TEMP:1)                              
                DISPLAY 'ALAMAKOTASTATEKPOLOZONY'                       
                DISPLAY 'PLACE AT ROW: ' WS-ROW-POSITION                
                 'COLUMN: '  WS-COLUMN-POSITION-TEMP                    
                IF WS-ITER2 > 10 THEN                                   
                   DISPLAY 'WS-ITER2PRZEBIL10'                          
                END-IF                                                  
                ADD 1 TO WS-ITER2                                       
              END-IF                                                    
           END-PERFORM                                                  
           IF SO-SHIP-POSITION-VALID THEN                               
                PERFORM 2030-SAVE-THE-BOARD                             
                SET SO-SHIP-WAS-PLACED TO TRUE                          
                ADD 1 TO WS-SHIP-COUNTER                                
           ELSE                                                         
                SET SO-SHIP-WAS-NOT-PLACED TO TRUE                      
                PERFORM 2031-INITALIZE-THE-BOARD                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2237-COUNT-TYPES-OF-SHIPS                    
      * IF SHIP WAS PLACED SUCCESSFULLY THEN WE WILL INCREMENT          
      * VARIABLE THAT COUNTS AMOUNT OF SHIPS IN A GIVEN TYPE            
      *                                                                 
      *                                                                 
      ******************************************************************
       2237-COUNT-TYPES-OF-SHIPS.                                       
           DISPLAY 'SHIPWASPLACE NOW FIELD NUBMER: ' WS-FIELD-NUMBER    
                                                                        
           SET  SO-THIS-TYPE-SHIP-PLACED(WS-FIELD-NUMBER) TO TRUE       
           ADD  1 TO WS-THIS-TYPE-SHIP-COUNTER(WS-FIELD-NUMBER)         
           MOVE 0 TO WS-SHIP-COUNTER                                    
           DISPLAY '2237 SUMA TYPU: '                                   
                  WS-THIS-TYPE-SHIP-COUNTER(WS-FIELD-NUMBER)            
           .                                                            
      ******************************************************************
      *              2240-CALC-MAXIMAL-SHIP-NUMBER                      
      * AFTER THIS PARAGRAPH IS FINISHED                                
      * OUR ARRAY WILL STORE INFORMATION ABOUT                          
      * HOW MANY SHIPS CAN HAVE A GIVEN TYPE OF A SHIP                  
      * FOR EXAMPLE                                                     
      * SHIP WITH THE LENTH OF 5 CAN ONLY OCCUR ONCE                    
      * 4 - 2                                                           
      * 3 - 3                                                           
      * 2 - 4                                                           
      ******************************************************************
       2240-CALC-MAXIMAL-SHIP-NUMBER.                                   
           DISPLAY '2240 PARAGRAPH WAS PERFORMED'                       
           MOVE 4 TO WS-ITER19                                          
           PERFORM VARYING WS-ITER5 FROM 2 BY 1 UNTIL WS-ITER5 > 5      
                                                                        
             MOVE WS-ITER19 TO WS-MAXIMAL-AMOUNT-OF-SHIPS(WS-ITER5)     
                                                                        
             DISPLAY 'FOR  ' WS-ITER5 ' MAX IS '                        
              WS-MAXIMAL-AMOUNT-OF-SHIPS(WS-ITER5)                      
             SUBTRACT 1 FROM WS-ITER19                                  
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                     2241-PREPARE-SHIPS-ARRAY.                   
      ******************************************************************
       2241-PREPARE-SHIPS-ARRAY.                                        
           DISPLAY '2241 PREPARE SHIPS ARRAY PERFORMED'                 
           PERFORM 2240-CALC-MAXIMAL-SHIP-NUMBER                        
           PERFORM 2242-INIT-USER-SHIP-ARRAY                            
           .                                                            
      ******************************************************************
      *                  2242-INIT-USER-SHIP-ARRAY                      
      ******************************************************************
       2242-INIT-USER-SHIP-ARRAY.                                       
           DISPLAY '2242-INIT-SHIP-ARRAY PERFORMED '                    
           PERFORM VARYING WS-ITER19 FROM 2 BY 1 UNTIL WS-ITER19 > 5 
              MOVE 0 TO WS-THIS-TYPE-SHIP-COUNTER(WS-ITER19)            
              SET  SO-THIS-TYPE-NOT-PLACED(WS-ITER19) TO TRUE           
              DISPLAY ' WS-THIS-TYPE-SHIP-COUNTER (' WS-ITER19 ')'      
                        WS-THIS-TYPE-SHIP-COUNTER(WS-ITER19)            
              DISPLAY 'SW-IF-SHIP-PLACED(WS-ITER19) (' WS-ITER19 ') '   
                               SW-IF-SHIP-PLACED(WS-ITER19)             
           END-PERFORM                                                  
           .                                                            
      ******************************************************************
      *                2250-USER-CHOOSE-MULTIPLAYER                     
      * PARAGRAPH WILL PREPARE DATA AND WILL DISPLAY MULTIPLAYER MAP    
      * (THIS WILL BE THE MAP WHERE USER HAS TO PROVIDE HIS             
      * NICK                                                            
      *                                                                 
      ******************************************************************
       2250-USER-CHOOSE-MULTIPLAYER.                                    
           DISPLAY '2250 WAS PERFORMED '                                
           DISPLAY 'MULTIPLAYER  '                                      
                                                                        
           SET SO-MODE-MULTIPLAYER        TO TRUE                       
           DISPLAY '2103 SO-MODE-MULTIPLAYER IS TRUE '                  
           SET SO-USER-CHOOSE-GAME-MODE   TO TRUE                       
           SET SO-USER-DIDNT-PROVIDE-NICK TO TRUE                       
           MOVE LOW-VALUES TO MP0236O                                   
           MOVE LOW-VALUES TO NICKA                                     
           PERFORM 2201-SEND-THE-FIRST-MULTI-MAP                        
           .                                                            
      ******************************************************************
      *                2251-USER-CHOSE-SINGLEPLAYER                     
      * PARAGRAPH WILL PREPARE DATA AND WILL DISPLAY                    
      * SINGLE PLAYER MAP (MAP WHERE THE GAME GOES)                     
      ******************************************************************
       2251-USER-CHOSE-SINGLEPLAYER.                                    
           DISPLAY '2251 WAS PERFORMED '                                
           DISPLAY 'SINGLE PLAYER'      
           MOVE   'SINGLE PLAYER'         TO MSG2O                      
           SET SO-MODE-SINGLE-PLAYER      TO TRUE                       
           SET SO-USER-CHOOSE-GAME-MODE   TO TRUE                       
      * PARAGRAPH BELOW WILL DISPLAY A "SINGLE PLAYER MAP" TO THE       
      * USER, IT WILL ALSO PREPARE THE COMPUTER'S BOARD                 
                                                                        
           PERFORM 2005-PREPERE-MAP-OF-COMPUTER                         
                                                                        
           IF SO-INVALID-COMPUTER-MAP     OR                            
              SO-TRY-GENERATE-MAP-AGAIN   OR                            
              SO-NOT-ALL-SHIPS-ARE-PLACED THEN                          
             DISPLAY '2151 ERROR WHILE GENEREATING THE MAP '            
             PERFORM 2130-INITIALIZE-MAP                                
             MOVE 'ERROR WHILE GENERATEING COMPUTER MAP'                
                                          TO MSGO                       
           ELSE                                                         
             PERFORM 2107-SAVE-COMPUTER-BOARD                           
             PERFORM 2130-INITIALIZE-MAP                                
             MOVE 'PLEASE YOUR SHIPS AT THE LEFT SIDE BY USING "S" '    
                                          TO MSGO                       
           END-IF                                                       
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                2255-PREPARE-GAME-DATA                           
      ******************************************************************
       2255-PREPARE-GAME-DATA.                                          
              DISPLAY 'SO ENEMY FOUND '                                 
              MOVE T01-PLAYER-NICK-TEXT TO WS-ENEMY-NICK                
              MOVE WS-USER-NICK         TO WS-MINE-NICK                 
      * WE WILL DELETE OUR (THIS PROGRAM PLAYER) FROM THE DATABASE      
      * CORRESPONDING PROGRAM WILL DELETE HIS PLAYER ALSO               
      * ( IT IS DONE IN ORDER TO NOT ASSIGNS PLAYERS INTO OTHER         
      * GAME IN THE SAME TIME)                                          
              MOVE WS-MINE-NICK TO WS-USER-NICK                         
              PERFORM 7004-DELETE-PLAYER-FROM-BASE                      
      * THEN WE WILL GENERATE A RECORD IN T02_BATTLESHIP_BOARD TABLE    
      * OUR BOARD WILL BE EMPTY AT THIS POINT                           
              PERFORM 7006-INSERT-EMPTY-GAME-BOARD                      
      * WE WILL WAIT FOR THE 5 SECOND IN ORDER TO BE SURE THAT          
      * ENEMY ALSO CREATED THIS RECORD                                  
              PERFORM 2206-DELAY-FOR-5-SECONDS                          
      * NOW WE WILL CHECK IF CORRESPONING T02 TABL RECORD EXISTS        
              PERFORM 7007-CHECK-CORRESPONDING-BOARD                    
      * PARAGRAPH ABOVE WILL CHECK IF OUR ENEMY CREATED RECORD          
      * IN THE T02 TABLE (THIS TABLE WILL STORE INFO ABOUT IT'S         
      * HIS BOARD) ( IF THIS RECORD WAS NOT CREATED THEN                
      * THERE WAS A MISTAKE AND WE SHOULD REPEAT THE PROCESS OF         
      * FINDING THE PLAYER)                                             
           .                                                            
      ******************************************************************
      *                2256-PREPARE-MAIN-TABLE                          
      ******************************************************************
       2256-PREPARE-MAIN-TABLE.                                         
                DISPLAY 'SO-OUR-ENEMY-MADE-RECORD IS TRUE '             
      * THE MAIN TABLE WILL STORE INFO, WHO HAS THE TURN RIGHT NOW      
                PERFORM 7008-INSERT-INTO-MAIN-TABLE                     
      * BOTH OF THE PROGRAMS WILL EXECUTE PARAGRAPH ABOVE               
      * SO THERE WILL BE 2 MAIN TABLE RECORDS                           
      * IN ORDER TO LEAVE ONLY 1 OF THEM WE WILL TRY TO DELETE          
      * THE RECORD WE JUST INSERTED BUT ONLY IF RECORD OF OTHER         
      * PLAYER EXISTS IF IT'S NOT THEN WE WILL DO NOTHING               
      * WHILE SECOND LAYER WILL DO THE SAME HE WILL DELETE              
      * HIS RECORD BECAUSE OUR ALREADY EXISTS                           
                PERFORM 7009-DELT-MAIN-IF-OTHER-EXIST                   
           .                                                            
      ******************************************************************
      *                2257-DISPLAY-MESG-AND-WAIT                       
      ******************************************************************
       2257-DISPLAY-MESG-AND-WAIT.                                      
           MOVE 'WAIT FOR YOUR TURN' TO MSGO                            
           PERFORM 2100-SEND-THE-MAP               
           PERFORM 2181-WAIT-FOR-HALF-SECOND                            
           MOVE 'WAIT FOR YOUR TURN' TO MSGO                            
           PERFORM 2100-SEND-THE-MAP                                    
                                                                        
           SET SO-WAIT-FOR-OPPONENT TO TRUE                             
      *    PERFORM 2222-CHECK-UNTIL-OUR-TURN                            
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                    2258-DELETE-GAME-DATA                        
      ******************************************************************
       2258-DELETE-GAME-DATA.                                           
           DISPLAY 'PLAYER WAS NOT FOUND '                              
           MOVE 'THE PLAYER COULD NOT BE FOUND' TO MSG3O                
           PERFORM 7020-DELETE-THIS-GAME-DATA                           
           PERFORM 2201-SEND-THE-FIRST-MULTI-MAP                        
           PERFORM 2201-SEND-THE-FIRST-MULTI-MAP                        
           DISPLAY 'AFTER 2201 SEND MAP IN 7002 '                       
           .                                                            
      ******************************************************************
      *              2260-CHECK-PLAYER-WITH-TURN                        
      * PARAGRAPH IS CALLED IN ORDER TO CHECK WHAT WAS IN VARIABLE      
      * THAT SHOULD STORE NAME OF THE PLAYER WITH TURN                  
      *                                                                 
      * EXCEPT FOR PLAYER WITH TURN THIS VARIABLE CAN STORE             
      * SYBMLS THAT INDICATES DIFFERNT STATES                           
      *                                                                 
      *     1. ENEMY WON                                                
      *     2. ENEMY IS DISCONNECTED                                    
      *     3. ENEMY WAS INACTIVE (DIDN'T MAKE A MOVE IN TIME)          
      ******************************************************************
       2260-CHECK-PLAYER-WITH-TURN.                                     
           EVALUATE T03-PLAYER-WITH-TURN                                
      * WHEN IT IS OUR TURN                                             
           WHEN   T03-FIRST-PLAYER                                      
                 SET SO-OUR-TURN             TO TRUE 
                 SET SO-ENEMY-IS-CONNECTED   TO TRUE                    
      * WHEN IT IS ENEMY TURN                                           
           WHEN   T03-SECOND-PLAYER                                     
                 SET SO-ENEMY-IS-CONNECTED   TO TRUE                    
                 SET SO-ENEMY-TOUR           TO TRUE                    
      * WHEN ENEMY IS DISCONNECTED                                      
           WHEN   CT-DISCONECTED-USER-SYMBOL                            
                 DISPLAY '7010 ENEMY IS DISCONNECTED '                  
                 SET SO-ENEMY-IS-DISCONECTED TO TRUE                    
                 SET SO-GAME-SHOULD-END      TO TRUE                    
      * WHEN ENEMY WON THE GAME                                         
           WHEN   CT-USER-WIN-SYMBOL                                    
                 DISPLAY '7010 ENEMY WON THE GAME '                     
                 SET SO-ENEMY-WON-THE-GAME   TO TRUE                    
                 SET SO-GAME-SHOULD-END      TO TRUE                    
      * THIS WHEN WILL BE TRUE WHEN "THIS" USER                         
      * WAS INACTIVE (DIDNT MAKE A MOVE)                                
           WHEN CT-ENEMY-INACTIVE-SYMBOL                                
                 SET SO-PLAYER-WAS-INACTIVE  TO TRUE                    
                 SET SO-GAME-SHOULD-END      TO TRUE                    
           WHEN OTHER                                                   
             DISPLAY 'OHTER IN 7010 '                                   
             PERFORM 3001-ERROR-EXIT                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                    2303-PREP-DATA-BEFORE-WAITING                
      * PARAGRAPH IS CALLED WHEN PROGRAM NEEDS TO SAVE ALL THE DATA     
      * THAT WERE USED BY THE USER, IN ORDER TO PREPARE THE GAME        
      * TO WAIT FOR THE ENEMY MOVE                                      
      ******************************************************************
       2303-PREP-DATA-BEFORE-WAITING.                                   
           PERFORM 2216-GET-USER-BOARD-TO-SCREEN                        
           SET SO-GET-ENEMY-BOARD  TO TRUE                              
           PERFORM 7011-GET-THE-GAME-BOARD                              
           PERFORM 2214-MOVE-BOARDS-TO-SCREEN      
           .                                                            
      ******************************************************************
      *              2304-PROCESS-WHEN-ENTER                            
      * PARAGRAPH WILL BE PERFORMED IF USER WILL PRESS ENTER KEY        
      ******************************************************************
       2304-PROCESS-WHEN-ENTER.                                         
           DISPLAY '2304-PROCESS-WHEN-ENTER PERFORMED'                  
      * IF USER ALREADY CHOOSE IF HE WANTS TO PLAY IN SINGLEPLAYER      
      * OR MULTIPLAYER                                                  
      *                                                                 
           IF SO-USER-CHOOSE-GAME-MODE THEN                             
                                                                        
             SET SO-DONT-WAIT-FOR-OPPONENT  TO TRUE                     
                                                                        
             IF SO-MODE-MULTIPLAYER  AND NOT SO-GAME-SHOULD-END         
                                                        THEN            
      * PARAGRAPH WILL VALIDATE USER INTPUT WHEN GAME MODE IS           
      * MULTIPLAYER                                                     
               PERFORM 2233-PROCESS-DATA-WHEN-MULTIP                    
             ELSE                                                       
                                                                        
      * PARAGRAPH WILL VALIDATE USER INTPUT WHEN GAME MODE IS           
      * SINGLE PLAYER                                                   
               PERFORM 2234-PROCESS-DATA-WHEN-SINGLP                    
             END-IF                                                     
      * THIS IF STATEMENT WILL ISSUE A PROCESS THAT WILL GO IN LOOP     
      * AND WILL BE CHECKING IF THIS IS OUR TURN                        
      * IF AFTER 100 SECONDS OUR OPPONENT WONT MAKE A MOVE THEN         
      * GAME WILL BE TERMINATED                                         
             IF SO-WAIT-FOR-OPPONENT THEN                               
                   PERFORM 2222-CHECK-UNTIL-OUR-TURN                    
             END-IF                                                     
           ELSE                                                         
      * THOSE STATEMENTS WILL BE EXECUTED ONLY WHEN USER PRESSED        
      * ENTER ON THE CHOICE FIELD ( HERE WE WILL VALIDATE               
      * IF USER CHOICE WAS VALID AND IF SO WE WILL DECIDE WHAT     
      * THE NEXT USER'S MAP SHOULD BE )                                 
             DISPLAY '2104 WHEN ENTER'                                  
             PERFORM   2102-GET-USER-CHOICE                             
             PERFORM   2103-PROCESS-USER-CHOICE                         
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2305-PROCESS-WHEN-F3-PRESSED                  
      * PARAGRAPH WILL BE PERFORMED IF USER WILL PRESS 'F3' BUTTON      
      ******************************************************************
       2305-PROCESS-WHEN-F3-PRESSED.                                    
           DISPLAY '2305-PROCESS-WHEN-F3-PRESSED PERFORMED'             
           SET SO-ENEMY-IS-DISCONECTED TO TRUE                          
           DISPLAY 'SW-IF-USER-IS-CONNECTED' SW-IF-USER-IS-CONNECTED    
           PERFORM 7012-SWITCH-THE-TURN                                 
           DISPLAY '2104 WHEN F3'                                       
           SET SO-FINAL-TERMINATION TO TRUE                             
           DISPLAY 'SW-WHAT-TYPE-OF-END ' SW-WHAT-TYPE-OF-END           
                                                                        
           .                                                            
      ******************************************************************
      *                   2306-PROCESS-WHEN-OTHER-KEY                   
      * PARAGRAPH WILL BE CALLED IF USER WILL PRESS KEY THAT            
      * DONT HAVE ANY ACTION ASSIGNED                                   
      ******************************************************************
       2306-PROCESS-WHEN-OTHER-KEY.                                     
           DISPLAY '2306-PROCESS-WHEN-OTHER-KEY PERFORMED'              
           IF SO-USER-CHOOSE-GAME-MODE THEN                             
             DISPLAY '2002  OTHER KEY '                                 
             MOVE    'INVALID KEY '    TO MSGO                          
             MOVE    DFHCOMMAREA       TO WS-COMMAREA                   
             PERFORM 2115-MOVE-COMMAREA-TO-SCREEN                       
             MOVE    WS-COMMAREA       TO DFHCOMMAREA                   
                                                                        
             IF SO-USER-DIDNT-PLACED-SHIPS                              
                DISPLAY 'SO-USER-DIDNT-PLACED-SHIPS TRUE NO ACTION '    
             ELSE                                                       
                DISPLAY 'SO-USER DID PLACE THE SHIPS'                   
                PERFORM 2105-PROTECT-USER-FIELDS                        
             END-IF                                                     
             PERFORM 2100-SEND-THE-MAP                                  
                                                                        
           ELSE                                                         
      * THIS PEICE OF CODE WILL BE PERFORMED WHEN WE ARE                
      * ON THE CHOICE MAP AND USER PRESSED INVALID KEY( NOT ENTER OR F3)
                                                                        
             DISPLAY '2104 WHEN OTHER'                                  
             MOVE    'INVALID KEY'     TO MSG2O                         
             DISPLAY 'MSG2O: '           MSG2O                          
             PERFORM 2101-SEND-THE-CHOICE-MAP                           
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2308-SEND-MAP-OF-COMPUTER-HIT                 
      ******************************************************************
       2308-SEND-MAP-OF-COMPUTER-HIT.                                   
           IF SO-COMPUTER-MISSED OR SO-COMPUTER-HIT-SOMETHING           
             PERFORM 2121-COMMON-TO-USER-BOARD                          
             PERFORM 2115-MOVE-COMMAREA-TO-SCREEN                       
             PERFORM 2161-CHECK-IF-ENDGAME                              
             PERFORM 2105-PROTECT-USER-FIELDS                           
             IF WS-LAST-EIBCPOSN NOT  = 0 THEN                          
               PERFORM 2170-SEND-SINGLE-MAP-CURSOR                      
             ELSE                                                       
               PERFORM 2100-SEND-THE-MAP                                
             END-IF                                                     
             PERFORM 2181-WAIT-FOR-HALF-SECOND                          
           ELSE                                                         
             CONTINUE                                                   
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                  2309-GET-POSITION-ENEMY-SHIP                   
      * PARAGRAPH WILL PERFORM ANOTHER PARAGRAPHS DEPENDING ON THE      
      * FLAG INDICATING IN WHICH DIRECTION PROGRAM SHOULD SHOOT         
      * IF SO-RANDOM-SHOT IS TRUE THEN PARAGRAPH WILL CHOOSE A FIELD    
      * RANDOMLY IN LOOP AS MANY TIMES AS LONG WE ARE HITING SOMETHING  
      * INVALID                                                         
      ******************************************************************
       2309-GET-POSITION-ENEMY-SHIP.                                    
           EVALUATE TRUE                                                
             WHEN  SO-RANDOM-SHOT                                       
               DISPLAY 'RANDOM-SHOT WS-ITERX: '  WS-ITERX               
               PERFORM 2165-GET-RANDOM-SHOT-POS                         
                                                                        
               PERFORM 2175-CHECK-RANDOM-NEIGHBORS                      
               DISPLAY 'AFTER 2175 FALGA IF CONINUE: '                  
                        SW-IF-CONTINUATION-ALLOWED                      
             WHEN SO-RIGHT-SHOT                                         
               DISPLAY '2112 SO-RIGHT-SHOT'                             
               PERFORM 2149-GET-RIGHT-SIDE-SHOT-POS                     
             WHEN SO-LEFT-SHOT                                          
               DISPLAY '2112 SO-LEFT-SHOT'                              
               PERFORM 2150-GET-LEFT-SHOT-POSITION                      
             WHEN SO-TOP-SHOT                                           
               DISPLAY '2112 SO-TOP-SHOT'                               
               PERFORM 2151-GET-UPPER-SHOT-POSITION                     
             WHEN SO-BOTTOM-SHOT                                        
               DISPLAY '2112 SO-BOTTOM-SHOT'                            
               PERFORM 2152-GET-LOWER-SHOT-POSITION                     
             WHEN OTHER                                                 
                DISPLAY 'OTHER (ERROR) IN 2112 '                        
                SET SO-RANDOM-SHOT TO TRUE                              
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  2310-FIRE-AT-THAT-POSITION.                    
      ******************************************************************
       2310-FIRE-AT-THAT-POSITION.                                      
           DISPLAY 'WS ROW POSITION: '  WS-ROW-POSITION                 
           DISPLAY 'WS COLUMN POSITION: '  WS-COLUMN-POSITION           
           DISPLAY '2113  TUTAJ NIE BYLO STRZALU  '                     
                                                                        
           SET SO-VALID-POSITION-WAS-FOUND TO TRUE                      
           IF  SO-SHIP-FIELD  THEN                                      
      * WHEN COMPUTER HIT A SHIP                                        
              SET SO-COMPUTER-HIT-SOMETHING TO TRUE                     
                                                                        
              DISPLAY 'NA TEJ POZYCJI JEST  "S" '                       
      * IF THERE IS A SHIP WE HAVE TO CHECK IF THIS SHIP WAS            
      * COMPLEATLY DISTROYED OR JUST GOT HIT                            
                                                                        
              MOVE CT-HIT-SHOT-SYMBOL TO                                
                    WS-PROGRAM-ARRAY(WS-ROW-POSITION)(                  
                                            WS-COLUMN-POSITION:1)       
              PERFORM 2114-CHECK-IF-DISTROYED                           
              SET SO-IT-IS-SAME-SIDE-TURN   TO TRUE                     
              IF SO-DESTROYED-SHIP THEN                                 
                 SET SO-DESTROYED-SHOT TO TRUE                          
               ELSE                                                     
                 SET SO-HIT-SHOT       TO TRUE                          
               END-IF                                                   
           ELSE                                                         
      * WHEN COMPUTER MISSED                                            
               SET SO-COMPUTER-MISSED TO TRUE                           
               SET SO-MISSED-SHOT TO TRUE                               
               MOVE CT-MISSED-SHOT-SYMBOL TO                            
                   WS-PROGRAM-ARRAY(WS-ROW-POSITION)(                   
                                             WS-COLUMN-POSITION:1)      
               SET SO-TURN-CHANGES     TO TRUE                          
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 2320-CHECK-USERS-SHIPS                          
      *****************************************************************
       2320-CHECK-USERS-SHIPS.                                         
           DISPLAY '2032 SO-USER-DIDNT PLACED SHIPS '                  
           PERFORM 2241-PREPARE-SHIPS-ARRAY                            
           PERFORM 2033-VALIDATE-USER-SHIPS                            
           .                                                           
      *****************************************************************
      *                2321-START-THE-SHOT-PROCEDURE                   
      * THIS PARAGRAPH WILL PERFORM PARAGRAPHS DEPENDING ON THE FACT   
      * WHO HAS THE TURN RIGHT NOW                                     
      * (WHO SHOOTS COMPUTER OR USER)                                  
      *****************************************************************
       2321-START-THE-SHOT-PROCEDURE.                                  
           EVALUATE TRUE                                               
             WHEN SO-GAME-SHOULD-END                                   
                DISPLAY 'GAME SHOULD END    (NO ACTION) '              
             WHEN SO-IT-IS-USERS-TURN                                  
                DISPLAY '2032 SO THIS IS USER TURN '                   
                PERFORM 2116-USER-SHOOTS                               
                                                                       
             WHEN SO-IT-IS-COMPUTERS-TURN                              
                DISPLAY '2032THIS IS COMPUTER TURN '                   
                PERFORM 2112-COMPUTER-SHOOTS                           
                                                                       
             WHEN OTHER                                                
                DISPLAY 'OTHER IN 2032 '                               
                 MOVE 'OTHER IN 2032 ' TO MSGO                         
           END-EVALUATE                                                
           .                                                           
      *****************************************************************
      *                2322-COMPUTER-SHOTS-IN-LOOP                     
      * THANKS TO THAT PARAGRAPH COMPUTER WILL BE ALLWED TO SHOOT AT   
      * THE USER SHIPS AS LONG HE HITS HIM, IN THE MOMENT HE MISS THEN 
      * TURN WILL GO TO OUR USER                                       
      *                                                                
      * LOOP LOGIC IS CREATED BY CALLING 2321-START-THE-SHOT-PROCEDURE 
      * FROM HERE AS LONG AS IT IS NEEDED                               
      *                                                                 
      ******************************************************************
       2322-COMPUTER-SHOTS-IN-LOOP.                                     
           DISPLAY '2032 SO-TURN-CHANGED-TO-COMPUTER '                  
           SET SO-FINAL-WITH-COMMAREA TO TRUE                           
           DISPLAY 'SW-WHAT-TYPE-OF-END ' SW-WHAT-TYPE-OF-END           
                                                                        
           IF SO-USER-SHOT-POSITION-VALID THEN                          
             DISPLAY '2032 SO-USER-SHOT-POSITION-VALID '                
             PERFORM 2170-SEND-SINGLE-MAP-CURSOR                        
           ELSE                                                         
             DISPLAY '2032 NOT SO-USER-SHOT-POSITION-VALID '            
             PERFORM 2100-SEND-THE-MAP                                  
           END-IF                                                       
           PERFORM 2181-WAIT-FOR-HALF-SECOND                            
           MOVE WS-COMMAREA TO DFHCOMMAREA                              
           DISPLAY '2023 PERFORM 2000-PROCESS '                         
           PERFORM 2321-START-THE-SHOT-PROCEDURE                        
           .                                                            
      ******************************************************************
      *                   2329-COUNT-FAILED-LOOP-ITERS                  
      * IF STATEMENT BELOW WILL COUNT THE NUMBER OF ITERATIONS WHERE    
      * THERE WAS NO SHIP PLACED ON THE BOARD                           
      * IF THIS SITUATION STAYES FOR LONGER THAN 50 ITERATIONS THEN     
      * WE WILL STOP THIS PROCESS AND WE WILL START ONCE                
      * AGAIN                                                           
      *                                                                 
      * MAXIMAL NUMBER OF CYCLES OF THIS PROCESS IS 5                   
      *                                                                 
      * EXPLENATION:                                                    
      *                                                                 
      *   IF PARAGRAPH CANT FIND PLACE FOR THE SHIP FOR MORE THAT 50    
      * ITERATIONS OF THIS LOOP ( THIS LOOP WILL RANDOMLY CHOOSE POSITIO
      * AND ORIENTATION )                                               
      * THEN WE WILL CLEAR WHOLE BOARD AND WE WILL START THE PROCESS    
      * OF PLACING THE SHIPS ONCE AGAIN                                 
      *                                                                 
      * WE CAN REPEAT THIS PROCESS 5 TIMES ( IF THIS WONT BE SUCCESSFULL
      * THEN WE WILL FINISH THE TRANSACTION WITH ERROR )                
      ******************************************************************
       2329-COUNT-FAILED-LOOP-ITERS.                                    
           DISPLAY '2329 COUNT FIELD ITERS PERFORMED '                  
           IF SO-SHIP-WAS-PLACED THEN                                   
              DISPLAY '2329 RESET THE COUNTER'                          
              MOVE 0 TO WS-NON-PLACED-ITERATIONS                        
           ELSE                                                         
              DISPLAY '2329 ADD 1 COUNTER: ' WS-NON-PLACED-ITERATIONS   
              ADD  1 TO WS-NON-PLACED-ITERATIONS                        
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                    2330-CHECK-LOOP-ITERATIONS                   
      ******************************************************************
       2330-CHECK-LOOP-ITERATIONS.                                      
           ADD 1 TO WS-ITER1                                            
           IF WS-ITER1 > CT-MAXIMAL-LOOP-COUNTER THEN                   
              DISPLAY 'LOOP OVER MAXIMAL NUMBER  '                      
              SET     SO-INVALID-COMPUTER-MAP    TO TRUE                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                   2331-INITIALIZE-SHIPS-VARS                    
      * PARAGRAPH IS PERFORMED WHEN PROGRAM TRIES TO GENERATE           
      * A COMPUTER'S MAP AND THERE IS A NEED TO START THIS PROCESS      
      * ONCE AGAIN                                                      
      * THEN THIS PARAGRAPH IS CALLED TO RESET ALL NEEDED VARIABLES     
      ******************************************************************
       2331-INITIALIZE-SHIPS-VARS.                                      
           DISPLAY '2331-INITIALIZE-SHIPS-VARS PERFORMED '              
           PERFORM 2241-PREPARE-SHIPS-ARRAY                             
           MOVE    0                         TO WS-NON-PLACED-ITERATIONS
           MOVE    0                           TO WS-ITER1              
           MOVE    5                           TO WS-FIELD-NUMBER       
           MOVE    0                           TO WS-SHIP-COUNTER       
           PERFORM 2011-INITALIZE-FLAGS                                 
           PERFORM 2015-INITIALIZE-SCREEN-ARRAY                         
                                                                        
                                                                        
                                                                        
           .                                                            
      ******************************************************************
      *                       3000-FINAL                                
      ******************************************************************
       3000-FINAL.                                                      
           DISPLAY 'TYP DZIALANIA: (W 3000)  ' SW-PROGRAM-MODE          
           DISPLAY '3000-PROCESS '                                      
           EVALUATE TRUE                                                
           WHEN SO-FINAL-WITH-COMMAREA                                  
             PERFORM 3002-RETURN-WITH-COMMAREA                          
           WHEN SO-FINAL-TERMINATION                                    
             PERFORM 3003-FINAL-TERMINATION                             
                                                                        
           WHEN SO-TERMINATION-WITHOUT-MESS                             
      * THIS 'WHEN' WILL ONLY TERMINATE THE TRANSACTION                 
      * NO OTHER MESSAGE WILL BE DISPALYED FOR THE USER                 
      *                                                                 
      * TABLE T03_BATTLESHIP_MAIN_TABLE WILL ALSO BE                    
      * MODIFIED TO INDICATE THAT GAME ENDED                            
                                                                        
             PERFORM 3004-FINATL-WITHOUT-MESG                           
           WHEN OTHER                                                   
             PERFORM 3006-SEND-ERROR-WHEN-OTHER                         
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                     3001-ERROR-EXIT                             
      * PARAGRAPH IS CALLED IN SINGLE PLAYER AND MULITPLAYER MODE 
      *                                                                 
      *                                                                 
      * IF IT IS MULTIPLAYER PARAGRAPH WILL MODIFIY NAME OF THE PLAYER  
      * WITH TURN TO 'XXXXXX' THAT WILL INDICATE THAT USER WAS          
      * DISCONNECTED                                                    
      *                                                                 
      *                                                                 
      * PARAGRAPH WILL ALSO DISPLAY AN ERROR MESSAGE AND WILL           
      * TERMINATE THE TRANSACTION                                       
      ******************************************************************
       3001-ERROR-EXIT.                                                 
           IF SO-MODE-MULTIPLAYER THEN                                  
             SET SO-ENEMY-IS-DISCONECTED TO TRUE                        
             PERFORM 7012-SWITCH-THE-TURN                               
           END-IF                                                       
           EXEC CICS                                                    
             SEND TEXT FROM(WS-ERROR-MESSAGE)                           
             ERASE                                                      
           END-EXEC                                                     
                                                                        
           EXEC CICS                                                    
             RETURN                                                     
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                3002-RETURN-WITH-COMMAREA                        
      ******************************************************************
       3002-RETURN-WITH-COMMAREA.                                       
                                                                        
           DISPLAY 'RETURN WITH COMMAREA'                               
           EXEC CICS                                                    
             RETURN TRANSID('0227')                                     
             COMMAREA(WS-COMMAREA)                                      
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                     3003-FINAL-TERMINATION                      
      ******************************************************************
       3003-FINAL-TERMINATION.                                          
           DISPLAY 'FINAL TERMINATION   '                               
           EXEC CICS                                                    
             SEND TEXT FROM(CT-ERROR-MESSAGE)                           
             ERASE                                                      
           END-EXEC                                                     
           EXEC CICS                                                    
             RETURN                                                     
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                    3004-FINATL-WITHOUT-MESG                     
      ******************************************************************
       3004-FINATL-WITHOUT-MESG.                                        
           SET SO-ENEMY-IS-INACTIVE    TO TRUE                          
           PERFORM 7012-SWITCH-THE-TURN                                 
           EXEC CICS                                                    
             RETURN                                                     
           END-EXEC                                                     
           .                                                            
      ******************************************************************
      *                 3006-SEND-ERROR-WHEN-OTHER                      
      ******************************************************************
       3006-SEND-ERROR-WHEN-OTHER.                                      
           MOVE 'OTHER ERROR IN 3000 ' TO MSGO                          
           SET  SO-DISPLAY-ERROR TO TRUE                                
           PERFORM 2100-SEND-THE-MAP                                    
           .                                                            
      ******************************************************************
      *                7001-INSERT-NICK-INTO-DATABSE                    
      ******************************************************************
       7001-INSERT-NICK-INTO-DATABSE.                                   
                                                                        
           MOVE WS-USER-NICK     TO T01-PLAYER-NICK-TEXT     
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN TO T01-PLAYER-NICK-LEN                 
           EXEC SQL                                                     
             INSERT INTO T01_PLAYERS_NICKS(PLAYER_NICK)                 
                      VALUES(:T01-PLAYER-NICK)                          
           END-EXEC                                                     
           SET SO-USER-DIDNT-PROVIDE-NICK TO TRUE                       
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
                                                                        
              SET SO-USER-PROVIDED-NICK TO TRUE                         
              PERFORM 2223-CICS-SYNCPOINT                               
           WHEN SO-SQLCODE-NOT-UNIQUE                                   
              MOVE 'PROVIDE UNICE NICKNAME !!! ' TO MSG3O               
           WHEN OTHER                                                   
             SET SO-7001-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   7002-WAIT-UNTIL-PLAYER-FOUND                  
      * PARAGRAPH WILL SEARCH FOR THE OPPONENT FOR OUR USER             
      *                                                                 
      * IT THE PARAGRAPH THERE IS A LOOP WITH LOGIC:                    
      *  1. CHECK IF THERE IS ANY OTHER PLAYER IF SO BREAK THE LOOP     
      *  2. DELAY FOR 5 SECONDS                                         
      *                                                                 
      * IF LOOP HAS BEEN BROKEN THEN  WE WILL DELETE OURSELFS FROM      
      * THE T01 TABLE (THERE ARE PLAYERS WHO WANTS TO JOIN THE GAME)    
      * THEN WE WILL INSERT OUR DATA TO T02 TABLE (TABLE WILL STORE     
      * OUR BOARD,                                                      
      * AND AFTER 5 SECONDS WE WILL CHECK IF OUR ENEMY DID THE SAME     
      * (IF HE CREATED T02 TABLE RECORD)                                
      * IF SO WE WILL CREATE T03 TABLE RECORD (THERE WE WILL STORE      
      * INTO WHO GOT THE TURN RIGHT NOW                                 
      * AND THEN WE WILL DELETE THIS NEWLY INSERTED RECORD              
      * BUT ONLY IF OUR ENEMY ALREADY CREATED THIS REOCRD)              
      *                                                                 
      * IF ENEMY DIDNT CREATE A T02 RECORD THEN WE WILL DELETE OUR      
      * T02 RECORD                                                      
      ******************************************************************
       7002-WAIT-UNTIL-PLAYER-FOUND.                                    
           DISPLAY ' 7002 PERFORMED '                                   
           SET SO-ENEMY-NOT-FOUND TO TRUE                               
           MOVE DFHBMPRO TO NICKA                                       
           MOVE 'SEARCHING FOR THE OPPONENT ' TO MSG3O                  
                                                                        
           PERFORM 2201-SEND-THE-FIRST-MULTI-MAP                        
           PERFORM 2181-WAIT-FOR-HALF-SECOND                            
           PERFORM 2201-SEND-THE-FIRST-MULTI-MAP                        
                                                                        
                                                                        
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
                            OR SO-ENEMY-FOUND                           
              DISPLAY 'IN THE 7002 LOOP '                               
              PERFORM 7003-SEARCH-FOR-OTHER-PLAYER                      
              PERFORM 2206-DELAY-FOR-5-SECONDS                          
           END-PERFORM                                                  
                                                                        
           IF SO-ENEMY-FOUND THEN                                       
              PERFORM 2255-PREPARE-GAME-DATA                            
                                                                        
      * IF ENEMY CRAETED A RECORD THAT WILL STORE HIS BOARD             
              IF SO-OUR-ENEMY-MADE-RECORD THEN                          
      * THIS PARAGRAPH WILL INSERT DATA INTO T03 TABLE                  
      * AND WILL DELETE THIS INSERTED DATA IF OUR ENEMY ALREADY         
      * DID THAT                                                        
                                                                        
                PERFORM 2256-PREPARE-MAIN-TABLE                         
                                                                        
                PERFORM 2130-INITIALIZE-MAP   
                SET SO-WE-DONT-HAVE-SHIPS TO TRUE                       
                PERFORM 7010-CHECK-WHOSE-TURN                           
                                                                        
                IF NOT SO-OUR-TURN THEN                                 
      * PARAGRAPH WILL SEND A MESSAGE TO NOTIFY THE USER HE HAS         
      * TO WAIT AND WILL ISSUE A PARAGRAPH THAT WILL                    
      * CHECK IF THE TURN IS OURS IN THE LOOP                           
                                                                        
                  PERFORM 2257-DISPLAY-MESG-AND-WAIT                    
                ELSE                                                    
                  MOVE 'PROVIDE THE SHIPS ' TO MSGO                     
                  PERFORM 2100-SEND-THE-MAP                             
                  MOVE 'PROVIDE THE SHIPS ' TO MSGO                     
                  PERFORM 2100-SEND-THE-MAP                             
                END-IF                                                  
              ELSE                                                      
                DISPLAY 'SO-OUR-ENEMY-MADE-RECORD IS FALSE'             
      * IF OUR ENEMY COULD NOT CREATE A RECORD WITH HIS BOARD           
      * THEN WE SHOULD DELETE A RECORD WITH OURS BOARD                  
      * IN CASE WE WILL NEED THIS RECORD WE WILL CREATE IT ONCE         
      * AGAIN                                                           
                PERFORM 7013-DELETE-THE-PLAYER-BOARD                    
              END-IF                                                    
           ELSE                                                         
      * PARAGRAPH WILL DELETE ALL DATA ABOUT THE GAME                   
      * USER WILL ALSO BE NOTIFIED THAT USER COULD NOT BE FOUND         
              PERFORM 2258-DELETE-GAME-DATA                             
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 7003-SEARCH-FOR-OTHER-PLAYER                    
      ******************************************************************
       7003-SEARCH-FOR-OTHER-PLAYER.                                    
           INITIALIZE   T01-PLAYER-NICK                                 
           EXEC SQL                                                     
              SELECT PLAYER_NICK   
              INTO :T01-PLAYER-NICK                                     
              FROM T01_PLAYERS_NICKS                                    
              WHERE PLAYER_NICK <> :WS-USER-NICK                        
                                                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
             SET SO-ENEMY-FOUND TO TRUE                                 
           WHEN SO-SQLCODE-NOT-FOUND                                    
             CONTINUE                                                   
                                                                        
           WHEN OTHER                                                   
             SET SO-7003-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                 7004-DELETE-PLAYER-FROM-BASE                    
      ******************************************************************
       7004-DELETE-PLAYER-FROM-BASE.                                    
           DISPLAY '7004 PERFORMED'                                     
           MOVE WS-USER-NICK TO T01-PLAYER-NICK-TEXT                    
           MOVE LENGTH OF WS-USER-NICK TO T01-PLAYER-NICK-LEN           
           EXEC SQL                                                     
             DELETE T01_PLAYERS_NICKS WHERE                             
               PLAYER_NICK = :WS-USER-NICK                              
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF SO-SQLCODE-NORMAL THEN                                    
             PERFORM 2223-CICS-SYNCPOINT                                
           ELSE                                                         
             SET SO-7004-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-IF                                                       
           .                     
      ******************************************************************
      *              7006-INSERT-EMPTY-GAME-BOARD                       
      * THIS PARAGRAPH WILL CREATE A RECORD IN T02_BATTLESHIP_BOARD     
      *  IN THE FORMAT LIKE BELOW                                       
      *  MY-NICK, ENEMY-NICK, PLACE FOR THE BOARD                       
      *                                                                 
      * ENEMY OF THIS USER WILL ALSO CREATE A RECORD IN THIS TABLE      
      * AND FROM OUR PERSPECTIVE IT WILL LOOK LIKE THIS:                
      * ENEMY-NICK, MY-NICK, PLACE FOR THE BOARD                        
      *                                                                 
      * THANKS TO THIS 2 RECORDS PROGRAMS WILL BE ABLE TO GET           
      * THE ENEMY BOARD, AND WILL KNOW WHICH BOARD IS THEIRS            
      *                                                                 
      ******************************************************************
       7006-INSERT-EMPTY-GAME-BOARD.                                    
           DISPLAY '7006 PERFORMED '                                    
           DISPLAY '7006 FIRST PLAYER  : '    T02-FIRST-PLAYER-TEXT     
           DISPLAY '7006 SECOND PLAYSER:  '   T02-SECOND-PLAYER-TEXT    
           MOVE WS-MINE-NICK            TO T02-FIRST-PLAYER-TEXT        
           MOVE LENGTH OF WS-MINE-NICK  TO T02-FIRST-PLAYER-LEN         
           MOVE WS-ENEMY-NICK           TO T02-SECOND-PLAYER-TEXT       
           MOVE LENGTH OF WS-ENEMY-NICK TO T02-SECOND-PLAYER-LEN        
           MOVE SPACE                   TO T02-GAME-BOARD-TEXT          
           MOVE 100                     TO T02-GAME-BOARD-LEN           
           EXEC SQL                                                     
            INSERT INTO T02_BATTLESHIP_BOARD(                           
                            FIRST_PLAYER                                
                           ,SECOND_PLAYER                               
                           ,GAME_BOARD)                                 
                                                                        
                          VALUES(                                       
                           :T02-FIRST-PLAYER                            
                          ,:T02-SECOND-PLAYER                           
                          ,:T02-GAME-BOARD)                             
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE  
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              DISPLAY '7006 SQLCODE NORMAL '                            
              PERFORM 2223-CICS-SYNCPOINT                               
           WHEN SO-SQLCODE-NOT-UNIQUE                                   
              DISPLAY ' 7006 NOT UNIQUE    '                            
              MOVE 'STRANGE ERROR IN 7006' TO MSG3O                     
           WHEN OTHER                                                   
              DISPLAY ' 7006 ERROR PARA   '                             
              SET SO-7006-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                7007-CHECK-CORRESPONDING-BOARD.                  
      ******************************************************************
       7007-CHECK-CORRESPONDING-BOARD.                                  
           DISPLAY '7007 DATA: '                                        
                                                                        
           MOVE WS-MINE-NICK            TO T02-SECOND-PLAYER-TEXT       
           MOVE LENGTH OF WS-MINE-NICK  TO T02-SECOND-PLAYER-LEN        
           MOVE WS-ENEMY-NICK           TO T02-FIRST-PLAYER-TEXT        
           MOVE LENGTH OF WS-ENEMY-NICK TO T02-FIRST-PLAYER-LEN         
           DISPLAY 'T02-FIRST-PLYAER: '    T02-FIRST-PLAYER-TEXT        
           DISPLAY 'T02-SECOND-PLAYER: '  T02-SECOND-PLAYER-TEXT        
           EXEC SQL                                                     
             SELECT "A"                                                 
                INTO :WS-DUMMY-VALUE                                    
              FROM T02_BATTLESHIP_BOARD                                 
              WHERE FIRST_PLAYER = :T02-FIRST-PLAYER                    
              AND SECOND_PLAYER = :T02-SECOND-PLAYER                    
              FETCH FIRST ROW ONLY                                      
                                                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE        
           WHEN SO-SQLCODE-NORMAL                                       
              SET SO-OUR-ENEMY-MADE-RECORD TO TRUE                      
              DISPLAY 'ENEMY DIDNT MAKE THE RECORD '                    
           WHEN SO-SQLCODE-NOT-FOUND                                    
              SET SO-ENEMY-DIDNT-MAKE-RECORD TO TRUE                    
              MOVE 'ENEMY DIDNT CREATE A T02 RECORD ? ' TO MSG3O        
              DISPLAY 'ENEMY DIDNT CREATE A T02 RECORD ? '              
           WHEN OTHER                                                   
              SET SO-7007-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                  7008-INSERT-INTO-MAIN-TABLE                    
      * PARAGRAPH WILL INSERT DATA INTO T03_BATTLESHIP_MAIN_TABLE       
      *                                                                 
      * WE WILL CREATE A RECORD IN FORMAT:                              
      *                                                                 
      *  MY-NICK, ENEMY-NICK, NAME-OF-ENEMY-WITH-TURN                   
      *                                                                 
      ******************************************************************
       7008-INSERT-INTO-MAIN-TABLE.                                     
           MOVE T02-FIRST-PLAYER-TEXT TO T03-PLAYER-WITH-TURN-TEXT      
           MOVE T03-PLAYER-WITH-TURN-TEXT TO WS-USER-NICK               
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN TO T03-PLAYER-WITH-TURN-LEN            
           EXEC SQL                                                     
             INSERT INTO T03_BATTLESHIP_MAIN_TABLE(FIRST_PLAYER,        
                  SECOND_PLAYER,PLAYER_WITH_TURN)                       
              VALUES(                                                   
                  :T02-FIRST-PLAYER,                                    
                  :T02-SECOND-PLAYER,                                   
                  :T03-PLAYER-WITH-TURN)                                
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF SO-SQLCODE-NORMAL THEN  
                                                                      
              PERFORM 2223-CICS-SYNCPOINT                             
           ELSE                                                       
              MOVE '7008 INVALID SQLCODE' TO MSG3O                    
           END-IF                                                     
           .                                                          
      ****************************************************************
      *                7009-DELT-MAIN-IF-OTHER-EXIST                  
      * THIS PARAGRAPH WILL DELETE A RECORD WE JUST INSERTED          
      * ONLY WHEN THERE IS ANOTHER RECORD THAT HAS                    
      * FIRST-PLAYER EQUAL TO OUR SECOND PLAYER                       
      * AND SECOIND PLAYER EQUAL TO OUR FIRST PLAYER                  
      ****************************************************************
       7009-DELT-MAIN-IF-OTHER-EXIST.                                 
           EXEC SQL                                                   
              DELETE  T03_BATTLESHIP_MAIN_TABLE                       
               WHERE                                                  
                   FIRST_PLAYER = :T02-FIRST-PLAYER                   
                      AND                                             
                   SECOND_PLAYER = :T02-SECOND-PLAYER                 
                  AND EXISTS                                          
                  (                                                   
                     SELECT FIRST_PLAYER                              
                      FROM T03_BATTLESHIP_MAIN_TABLE                  
                      WHERE FIRST_PLAYER = :T02-SECOND-PLAYER         
                              AND                                     
                         SECOND_PLAYER = :T02-FIRST-PLAYER            
                  )                                                   
                                                                      
           END-EXEC                                                   
           EVALUATE TRUE                                              
           WHEN SO-SQLCODE-NORMAL                                     
           WHEN SO-SQLCODE-NOT-FOUND                                  
              PERFORM 2223-CICS-SYNCPOINT                             
           WHEN OTHER                                                 
              SET SO-7009-PARA TO TRUE     
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                       7010-CHECK-WHOSE-TURN                     
      * T03-PLAYER-WITH-TURN WILL STORE NICK OF THE PLAYER THAT         
      * HAS THE TURN RIGHT NOW                                          
      ******************************************************************
       7010-CHECK-WHOSE-TURN.                                           
           DISPLAY 'SO-7010 PERFORMED '                                 
           MOVE WS-MINE-NICK TO T03-FIRST-PLAYER-TEXT                   
           MOVE WS-MINE-NICK TO WS-USER-NICK                            
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN TO T03-FIRST-PLAYER-LEN                
                                                                        
           MOVE WS-ENEMY-NICK TO T03-SECOND-PLAYER-TEXT                 
           MOVE WS-ENEMY-NICK TO WS-USER-NICK                           
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN TO T03-SECOND-PLAYER-LEN               
           INITIALIZE T03-PLAYER-WITH-TURN                              
           EXEC SQL                                                     
             SELECT PLAYER_WITH_TURN                                    
              INTO :T03-PLAYER-WITH-TURN                                
                                                                        
             FROM T03_BATTLESHIP_MAIN_TABLE                             
             WHERE (FIRST_PLAYER = :T03-FIRST-PLAYER                    
                              AND                                       
                   SECOND_PLAYER = :T03-SECOND-PLAYER )                 
                              OR                                        
                   (FIRST_PLAYER = :T03-SECOND-PLAYER                   
                              AND                                       
                   SECOND_PLAYER = :T03-FIRST-PLAYER  )                 
             FETCH FIRST ROW ONLY                                       
                                                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE      
           WHEN SO-SQLCODE-NORMAL                                       
               PERFORM 2223-CICS-SYNCPOINT                              
           WHEN SO-SQLCODE-NOT-FOUND                                    
              DISPLAY 'MAIN T03 TABLE NOT FOUND(ERROR) '                
              PERFORM 3001-ERROR-EXIT                                   
           WHEN OTHER                                                   
              SET SO-7010-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
                                                                        
      * PARAGRAPH WILL CHECK WHAT IS STORED IN A VARIABLES              
      * THAT SHOULD STORE NAME OF THE PLAYER WITH TURN                  
      * EXECEPT FOR, OUR USER AND ENEMY USER IT CAN STORE:              
      *                                                                 
      *   1. DISCONNECTED SYMBOL                                        
      *      WHEN OUR ENEMY WAS DISCONNECTED FROM THE GAME              
      *   2. WIN SYMBOL                                                 
      *      WHEN WE WERE DEFEATED ( ENEMY WON)                         
      *   3. INACTIVE SYMBOL                                            
      *      OUR ENEMY DIDNT MAKE A MOVE ON TIME                        
      *                                                                 
      * DEPENDING ON WHAT IS OUR SITUTATION PARAGRAPH BELOW WILL        
      * SET FLAGS, THAT WILL DETERMINE WHAT TO DO NEXT                  
      *                                                                 
           PERFORM 2260-CHECK-PLAYER-WITH-TURN                          
           .                                                            
      ******************************************************************
      *                7011-GET-THE-GAME-BOARD                          
      * THIS PARAGRAPH WILL CHECK IF WE PROVIDED SHIPS OR NOT           
      ******************************************************************
       7011-GET-THE-GAME-BOARD.                                         
           IF SO-GET-THIS-USER-BOARD       THEN                         
      * WHEN PROGRAM WANTS TO GET THIS USER BOARD                       
             PERFORM 2061-PREPARE-USER-BOARD-DATA                       
           ELSE                                                         
      * WHEN PROGRAM WANTS TO GET ENEMY'S BOARD
                                                                        
             PERFORM 2062-PREPARE-ENEMY-BOARD-DATA                      
                                                                        
           END-IF                                                       
           INITIALIZE  T02-GAME-BOARD                                   
           EXEC SQL                                                     
              SELECT GAME_BOARD                                         
               INTO :T02-GAME-BOARD                                     
                                                                        
              FROM T02_BATTLESHIP_BOARD                                 
              WHERE FIRST_PLAYER  = :T02-FIRST-PLAYER                   
              AND  SECOND_PLAYER = :T02-SECOND-PLAYER                   
                                                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
              DISPLAY '7011 T02-GAME-BOARD-TEXT: '                      
                       T02-GAME-BOARD-TEXT                              
      * HERE WE WILL CHECK IF THIS USER'S BOARD IS EMPTY OR NOT         
      * IF BOARD IS EMPTY IT MEANS THAT USER DIDNT PROVIDE SHIPS        
              IF T02-GAME-BOARD-TEXT = SPACE OR LOW-VALUES THEN         
                  SET SO-WE-DONT-HAVE-SHIPS TO TRUE                     
              ELSE                                                      
                  SET SO-WE-HAVE-THE-SHIPS TO TRUE                      
              END-IF                                                    
           WHEN SO-SQLCODE-NOT-FOUND                                    
              DISPLAY '7011 SQLCODE NOT FOUND '                         
              PERFORM 3001-ERROR-EXIT                                   
           WHEN OTHER                                                   
             SET SO-7011-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   7012-SWITCH-THE-TURN                          
      * THIS PARAGRAPH IS PERFORMED IN ORDER TO SWITH THE TURN          
      *                                                                 
      * IF THIS WAS TURN OF PLAYER A THEN THE TURN GOES TO THE          
      * PLAYER 'B'                                                      
      *                                                                 
      * THIS PARAGRAPH CAN ALSO MOVE 'XXXXXX' SYMBOL TO THE             
      * VARIABLE THAT INDICATES WHO HAS THE TURN                        
      *                                                                 
      * THIS VARIABLE WILL REPRESENT THAT ONE OF THE PLAYERS IS         
      * DISCONNECTED                                                    
      ******************************************************************
       7012-SWITCH-THE-TURN.                                            
           DISPLAY 'SO-7012 PERFORMED   '                               
           IF SO-OUR-TURN THEN                                          
                                                                        
              DISPLAY '7012  OUR TURN ERFORMED   '                      
              MOVE WS-ENEMY-NICK TO WS-USER-NICK                        
              PERFORM 2210-GET-NICK-LENGTH                              
              MOVE WS-USER-NICK-LEN TO WS-USER-NAME-LEN                 
              MOVE WS-USER-NICK     TO WS-USER-NAME-TEXT                
           ELSE                                                         
                                                                        
              DISPLAY '7012  ENEMY TURN ERFORMED   '                    
              MOVE WS-MINE-NICK TO WS-USER-NICK                         
              PERFORM 2210-GET-NICK-LENGTH                              
              MOVE WS-USER-NICK-LEN TO WS-USER-NAME-LEN                 
              MOVE WS-USER-NICK     TO WS-USER-NAME-TEXT                
           END-IF                                                       
      * THOSE 3 PARAGRAPH BELOW WILL MARK "PLAYER WITH TURN"            
      * AS DISCONNECTED, WINNNER OR INACTIVE IF WE NEED TO DO THAT      
           PERFORM 2063-MARK-USER-AS-DISCONNECTED                       
           PERFORM 2064-MARK-USER-AS-WINNER                             
           PERFORM 2065-MARK-USER-AS-INACTIVE                           
                                                                        
           DISPLAY '7012 FIRST PLAYER: ' T03-FIRST-PLAYER               
           DISPLAY '7012 SECOND PLAYER: ' T03-SECOND-PLAYER             
           EXEC SQL                                                     
            UPDATE  T03_BATTLESHIP_MAIN_TABLE                           
            SET PLAYER_WITH_TURN = :WS-USER-NAME                        
            WHERE                                                       
            (  FIRST_PLAYER = :T03-FIRST-PLAYER                         
                    AND                                                 
               SECOND_PLAYER = :T03-SECOND-PLAYER  )                    
                   OR                                                   
            (  FIRST_PLAYER = :T03-SECOND-PLAYER                        
                    AND                                                 
               SECOND_PLAYER = :T03-FIRST-PLAYER   )                    
           END-EXEC                                                     
           MOVE SQLCODE TO SQLCODE-FORMAT                               
           DISPLAY '7012 SQLCODE FORMAT: ' SQLCODE-FORMAT               
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
             DISPLAY '7012 SQLCODE NORMAL  '                            
             PERFORM 2223-CICS-SYNCPOINT                                
           WHEN SO-SQLCODE-NOT-FOUND                                    
      * THIS PARAGRAPH CAN BE CALLED WHEN THERE IS NO RECORD            
      * CREATED (IF THIS IS THE CASE THEN WE WILL NOT DROP ANY ERROR )  
              DISPLAY '7012 SQLCODE NOT FOUND'                          
           WHEN  OTHER                                                  
              DISPLAY '7012 OTHER SQLCODE '                             
                SET SO-7012-PARA TO TRUE                                
                PERFORM 9000-DB2-ERROR                                  
           END-EVALUATE                                                 
           .                                                            
      ******************************************************************
      *                   7013-DELETE-THE-PLAYER-BOARD                  
      * PARAGRAPH WILL BE CALLED IF OUR ENEMY DIDN'T CREATE A           
      * DATABASE RECORD THAT WILL STORE HIS BOARD, IF HE DIDN'T THEN    
      * WE WILL DELETE RECORD THAT STORES OUR BOARD                     
      ******************************************************************
       7013-DELETE-THE-PLAYER-BOARD.  
           MOVE WS-MINE-NICK TO T02-FIRST-PLAYER-TEXT                   
           MOVE WS-MINE-NICK TO WS-USER-NICK                            
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN TO T02-FIRST-PLAYER-LEN                
                                                                        
           MOVE WS-ENEMY-NICK TO T02-SECOND-PLAYER-TEXT                 
           MOVE WS-ENEMY-NICK TO WS-USER-NICK                           
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN TO T02-SECOND-PLAYER-LEN               
           EXEC SQL                                                     
             DELETE T02_BATTLESHIP_BOARD                                
              WHERE FIRST_PLAYER = :T02-FIRST-PLAYER                    
                          AND                                           
                  SECOND_PLAYER = :T02-SECOND-PLAYER                    
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7013-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
                                                                        
           ELSE                                                         
             PERFORM 2223-CICS-SYNCPOINT                                
           END-IF                                                       
           .                                                            
      ******************************************************************
      *                 7014-SAVE-THE-BOARD-TO-DB2                      
      ******************************************************************
       7014-SAVE-THE-BOARD-TO-DB2.                                      
           STRING WS-SCREEN-TABLE(1),                                   
                  WS-SCREEN-TABLE(2),                                   
                  WS-SCREEN-TABLE(3),                                   
                  WS-SCREEN-TABLE(4),                                   
                  WS-SCREEN-TABLE(5),                                   
                  WS-SCREEN-TABLE(6),                                   
                  WS-SCREEN-TABLE(7),                                   
                  WS-SCREEN-TABLE(8),   
                  WS-SCREEN-TABLE(9),                                   
                  WS-SCREEN-TABLE(10)                                   
           DELIMITED BY SIZE                                            
           INTO T02-GAME-BOARD-TEXT                                     
           END-STRING                                                   
           DISPLAY '7014 CALY REKORD: '                                 
           DISPLAY WS-SCREEN-TABLE(1)                                   
           DISPLAY WS-SCREEN-TABLE(2)                                   
           DISPLAY WS-SCREEN-TABLE(3)                                   
           DISPLAY WS-SCREEN-TABLE(4)                                   
           DISPLAY WS-SCREEN-TABLE(5)                                   
           DISPLAY WS-SCREEN-TABLE(6)                                   
           DISPLAY WS-SCREEN-TABLE(7)                                   
           DISPLAY WS-SCREEN-TABLE(8)                                   
           DISPLAY WS-SCREEN-TABLE(9)                                   
           DISPLAY WS-SCREEN-TABLE(10)                                  
                                                                        
                                                                        
      * THE BOARD IS ALWAYS (10 X 10) SO ITS LENGTH WILL BE ALWAYYS     
      * EQUAL TO 100                                                    
           MOVE 100 TO T02-GAME-BOARD-LEN                               
           PERFORM 7016-UPDATE-THE-BOARD-TABLE                          
           .                                                            
      ******************************************************************
      *                   7015-SAVE-THE-BOARD-TO-DB2                    
      * AT THE END OF THE 2116 PARAGRAPH PROGRAM SAVES                  
      * ENEMY (COMPUTER) BOARD TO THE WS-COMPUTER-BOARD-LINE ARRAY      
      * WE WILL NOW USE THIS ARRAY IN ORDER TO SAVE DATA TO THE DATABASE
      *                                                                 
      * THIS PARAGRAPH WILL ALSO MOVE ENEMY DATA ONTO OUR SCREEN        
      *                                                                 
      * (DATA IN WS-COMPUTER-BOARD-LINE IN THIS PLACE STORES ONLY       
      * OUR ENEMY BOARD (NOT THE COMPUTER BOARD ) )                     
      ******************************************************************
       7015-SAVE-THE-BOARD-TO-DB2.                                      
           DISPLAY '7015 PERFORMED '  
           PERFORM VARYING WS-ITER5 FROM 1 BY 1 UNTIL WS-ITER5 > 10     
              MOVE  WS-COMPUTER-BOARD-LINE(WS-ITER5) TO                 
                                    POLEKO(WS-ITER5)                    
              INSPECT POLEKO(WS-ITER5) REPLACING ALL 'S' BY ' '         
           END-PERFORM                                                  
                                                                        
           STRING  WS-COMPUTER-BOARD-LINE(1)                            
                   WS-COMPUTER-BOARD-LINE(2)                            
                   WS-COMPUTER-BOARD-LINE(3)                            
                   WS-COMPUTER-BOARD-LINE(4)                            
                   WS-COMPUTER-BOARD-LINE(5)                            
                   WS-COMPUTER-BOARD-LINE(6)                            
                   WS-COMPUTER-BOARD-LINE(7)                            
                   WS-COMPUTER-BOARD-LINE(8)                            
                   WS-COMPUTER-BOARD-LINE(9)                            
                   WS-COMPUTER-BOARD-LINE(10)                           
           DELIMITED BY SIZE                                            
           INTO T02-GAME-BOARD-TEXT                                     
           END-STRING                                                   
           MOVE 100 TO T02-GAME-BOARD-LEN                               
           PERFORM 7016-UPDATE-THE-BOARD-TABLE                          
           .                                                            
      ***************************************************************** 
      *                 7016-UPDATE-THE-BOARD-TABLE                     
      ******************************************************************
       7016-UPDATE-THE-BOARD-TABLE.                                     
           DISPLAY '7016 PERFORMED: '                                   
           DISPLAY '7016 FIRST: '  T02-FIRST-PLAYER-TEXT                
           DISPLAY '7016 SECOND: ' T02-SECOND-PLAYER-TEXT               
           EXEC SQL                                                     
             UPDATE T02_BATTLESHIP_BOARD                                
               SET GAME_BOARD = :T02-GAME-BOARD                         
                                                                        
             WHERE FIRST_PLAYER  = :T02-FIRST-PLAYER                    
             AND  SECOND_PLAYER = :T02-SECOND-PLAYER       
                                                                        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           IF NOT SO-SQLCODE-NORMAL THEN                                
             SET SO-7016-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           ELSE                                                         
             PERFORM 2223-CICS-SYNCPOINT                                
             DISPLAY ' 7016 THE BOARD WAS SUCCESSFULLY SAVED '          
           END-IF                                                       
           .                                                            
      ***************************************************************** 
      *                    7017-DELETE-THIS-NICK-DATA                   
      * THIS PARAGRAPH WILL DELETE ANY RECORD FROM T02_BATTLESHIP_BOARD 
      * AND T02_BATTLESHIP_MAIN_TABLE TABLES                            
      * THAT ARE CONNECTED WITH OUR NICK                                
      *                                                                 
      *                                                                 
      * THAT IS NEEDED IN ORDER TO AVOID DATA ERRORS                    
      ******************************************************************
       7017-DELETE-THIS-NICK-DATA.                                      
           PERFORM 7018-DELETE-FROM-BOARD-TABLE                         
           PERFORM 7019-DELETE-GAME-MAIN-TABLE                          
           .                                                            
      ***************************************************************** 
      *                 7018-DELETE-FROM-BOARD-TABLE                    
      * PARAGRAPH DELETES DATA FROM T02_BATTLESHIP_BOARD                
      *                                                                 
      * THIS PARAGRAPH WILL BE CALLED AFTER USER PROVIDES HIS NICK      
      *                                                                 
      ******************************************************************
       7018-DELETE-FROM-BOARD-TABLE.                                    
           EXEC SQL                                                     
             DELETE T02_BATTLESHIP_BOARD                                
              WHERE FIRST_PLAYER = :T01-PLAYER-NICK                     
               OR  SECOND_PLAYER = :T01-PLAYER-NICK        
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
           WHEN SO-SQLCODE-NOT-FOUND                                    
            PERFORM 2223-CICS-SYNCPOINT                                 
           WHEN OTHER                                                   
             SET SO-7018-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ***************************************************************** 
      *                7019-DELETE-GAME-MAIN-TABLE                      
      * PARAGRAPH IS CALLED IN ORDER TO DELETE RECORDS FROM MAIN        
      * TABLE THAT COULD HAVE USER'S NICK                               
      *                                                                 
      * THIS PARAGRAPH WILL MAKE SURE THAT THERE IS NO SUCH RECORD      
      *                                                                 
      * ( PROGRAM JUST DELTES DATA OF THE OLD GAME IF THERE WAS ANY)    
      *                                                                 
      ******************************************************************
       7019-DELETE-GAME-MAIN-TABLE.                                     
           EXEC SQL                                                     
             DELETE T03_BATTLESHIP_MAIN_TABLE                           
              WHERE FIRST_PLAYER = :T01-PLAYER-NICK                     
               OR  SECOND_PLAYER = :T01-PLAYER-NICK                     
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
           WHEN SO-SQLCODE-NOT-FOUND                                    
             PERFORM 2223-CICS-SYNCPOINT                                
           WHEN OTHER                                                   
             SET SO-7019-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE  
           .                                                            
      ***************************************************************** 
      *                  7020-DELETE-THIS-GAME-DATA                     
      * THOSE 3 PARAGRAPS WILL DELETE ALL POSSIBLE DATA ABOUT           
      * THE GAME                                                        
      *                                                                 
      * FROM ALL TABLES THAT ARE USED IN THE APPLICATION                
      ******************************************************************
       7020-DELETE-THIS-GAME-DATA.                                      
           MOVE WS-ENEMY-NICK TO WS-DB2-ENEMY-NICK-TEXT                 
           MOVE WS-ENEMY-NICK TO WS-USER-NICK                           
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN TO WS-DB2-ENEMY-NICK-LEN               
                                                                        
           MOVE WS-MINE-NICK TO WS-DB2-USER-NICK-TEXT                   
           MOVE WS-MINE-NICK TO WS-USER-NICK                            
           PERFORM 2210-GET-NICK-LENGTH                                 
           MOVE WS-USER-NICK-LEN TO WS-DB2-USER-NICK-LEN                
                                                                        
                                                                        
           PERFORM 7021-DELETE-FROM-NICK-TABLE                          
           PERFORM 7022-DELETE-FROM-BOARD-TABLE                         
           PERFORM 7023-DELETE-FROM-MAIN-TABLE                          
           .                                                            
      ***************************************************************** 
      *              7021-DELETE-FROM-NICK-TABLE.                       
      * PARAGRAPH WILL DELETE DATA ABOUT US AND THE ENEMY FROM          
      * THE T01_PLAYERS_NICKS TABLE                                     
      *                                                                 
      * HERE WE WILL NOT DO ANYTHING IF SQL QUEUERY WILL RETURN         
      * SQLCODE OF 100 BECAUSE WE DONT KNOW IF THERE ARE ANY DATA       
      * IN THIS TABLE RIGHT NOW                                         
      * BUT WE WILL ISSUE THIS DELETE STATMEENT TO BE SURE  THAT        
      * THIS TABLE WILL BE CLEAR                                        
      ******************************************************************
       7021-DELETE-FROM-NICK-TABLE. 
           EXEC SQL                                                     
             DELETE T01_PLAYERS_NICKS WHERE                             
               PLAYER_NICK = :WS-DB2-ENEMY-NICK                         
                           OR                                           
               PLAYER_NICK = :WS-DB2-USER-NICK                          
           END-EXEC                                                     
                                                                        
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
           WHEN SO-SQLCODE-NOT-FOUND                                    
              CONTINUE                                                  
           WHEN OTHER                                                   
              SET SO-7021-PARA TO TRUE                                  
              PERFORM 9000-DB2-ERROR                                    
           END-EVALUATE                                                 
           .                                                            
      ***************************************************************** 
      *                  7022-DELETE-FROM-BOARD-TABLE                   
      * PARAGRAPH WILL DELETE DATA OF THE BOARD THAT WERE USED IN THIS  
      * GAME                                                            
      ******************************************************************
       7022-DELETE-FROM-BOARD-TABLE.                                    
           MOVE WS-DB2-ENEMY-NICK  TO T02-FIRST-PLAYER                  
           MOVE WS-DB2-USER-NICK   TO T02-SECOND-PLAYER                 
            EXEC SQL                                                    
               DELETE T02_BATTLESHIP_BOARD                              
               WHERE                                                    
                     FIRST_PLAYER  = :T02-FIRST-PLAYER                  
               OR    FIRST_PLAYER  = :T02-SECOND-PLAYER                 
               OR    SECOND_PLAYER = :T02-FIRST-PLAYER                  
               OR    SECOND_PLAYER = :T02-SECOND-PLAYER                 
            END-EXEC                                                    
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL
           WHEN SO-SQLCODE-NOT-FOUND                                    
             CONTINUE                                                   
           WHEN OTHER                                                   
             SET SO-7022-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ***************************************************************** 
      *                 7023-DELETE-FROM-MAIN-TABLE                     
      ******************************************************************
       7023-DELETE-FROM-MAIN-TABLE.                                     
           EXEC SQL                                                     
             DELETE  T03_BATTLESHIP_MAIN_TABLE                          
               WHERE                                                    
                       FIRST_PLAYER  = :T02-FIRST-PLAYER                
                 OR    FIRST_PLAYER  = :T02-SECOND-PLAYER               
                 OR    SECOND_PLAYER = :T02-FIRST-PLAYER                
                 OR    SECOND_PLAYER = :T02-SECOND-PLAYER               
           END-EXEC                                                     
           MOVE SQLCODE TO SW-SQLCODE                                   
           EVALUATE TRUE                                                
           WHEN SO-SQLCODE-NORMAL                                       
           WHEN SO-SQLCODE-NOT-FOUND                                    
             CONTINUE                                                   
           WHEN OTHER                                                   
             SET SO-7023-PARA TO TRUE                                   
             PERFORM 9000-DB2-ERROR                                     
           END-EVALUATE                                                 
           .                                                            
      ***************************************************************** 
      *                      9000-DB2-ERROR                             
      ******************************************************************
       9000-DB2-ERROR.                                                  
           MOVE SQLCODE TO SQLCODE-FORMAT                               
           MOVE SQLERRMC TO WS-SQLERRMC                                 
           DISPLAY 'DB2 ERROR '      
           DISPLAY 'IN STATEMENT ' SW-STATEMENT-ID        
           DISPLAY 'SQLCODE: '  SQLCODE-FORMAT            
           DISPLAY 'SQLERRMC: '  WS-SQLERRMC              
                                                          
           STRING 'DB2 ERORR SQLCODE: ' SQLCODE-FORMAT    
           DELIMITED BY SIZE                              
           INTO  MSG3O                                    
                                                          
           END-STRING                                     
           .                                              
                                                          
      ****************************************************
      *                        9100-ROLLBACK              
      ****************************************************
       9100-ROLLBACK.                                     
           EXEC CICS                                      
            SYNCPOINT ROLLBACK                            
           END-EXEC                                       
           PERFORM 2200-CHECK-EIBRESP                     
           .                                                                                 
                                       
                                    
                                               
             
             
                                  
                                
                                  
                         
                                          
                           
                                  
                                        
                                 
                                       
                                     
                          
           
      






     
                     
                   
                     
                                
   
                  
                
                                     
           
                            
     
                        
             
                             
                                 
                           
                                  
                                 
                                                
                                   
                                   
                          
                                            
       
                          
          

                          
                                              
                          
                
                        
                           
                
                                            
              
                                     
        
                           
                 
                       
                                         
                               
                                                  
                  
              
        

                   
       
          
        
            
          
                        
                                               
                                           
               
                                              
                     
                                 


               
             
    
                   
     

                      

                   
                                    
                           
                                 

             
   

                               
                                     

   
                            
                     

                           
                                                                      
                                                                        
                                                        
                                                   
