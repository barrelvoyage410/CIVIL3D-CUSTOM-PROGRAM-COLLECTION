; Define a command HELLO that calls a simple dialogue

(defun C:HELLO ()

    ; Load the DCL file "hello.dcl" and get the dialogue's handle

    (setq hello_id (load_dialog "hello.dcl"))

    ; If the load returned nil, stop and exit the program; else, 
    ; begin handling the dialogue named "hello" in the DCL file
    ; identified by the dialogue handle hello_id.

    (if 
        (not hello_id)
        nil
        (new_dialog "hello" hello_id)
    )

    ; Start and display the dialogue "hello" and begin accepting
    ; input from the user

    (start_dialog)

    ; When the user presses the "Ok" button, dismiss the dialogue

    (action_tile "ok" "(done_dialog)")

    ; Quiet exit at Command: prompt

    (prin1)
)
