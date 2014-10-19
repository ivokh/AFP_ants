import CodeGenerator

main = 
    do
        let code = unlines . map show . runCode $ start
        writeFile "test.ant" code

start :: Code
start = combine 
    defendHome
    wander
    wait

defendHome :: Code
defendHome = annotate "defendHome" $ combine
    (sense Ahead (jump "step1") next Home)
    (sense LeftAhead (jump "step1") next Home)
    (sense RightAhead (jump "step1") next Home)
    (mark 0 next)
    (move this this)
    (annotate "step1" $ combine
        (pickUp next next)
        (pickUp next next)
        (pickUp next next) 
        (pickUp next next) -- Wait untill the first marker is dropped
        (sense Ahead next (jump "step2") (Marker 0))  
        (mark 0 (jump "wait"))
    )
    (annotate "step2" $ combine
        (sense Ahead (jump "step4") next Home)
        (sense RightAhead next (jump "step3") (Marker 0))
        (move (jump "wait") next)
    )
    (annotate "step3" $ combine
        (sense LeftAhead next (jump "wander") (Marker 0))
        (move (jump "wait") next)     
    )
    (annotate "step4" $ combine
        (pickUp next next)
        (pickUp next next)
        (pickUp next next) -- Wait to make sure the second marker has been droppd
        (sense RightAhead next (jump "step5") (Marker 0))
        (move (jump "wait") next)
    )
    (annotate "step5" $ combine
        (sense LeftAhead next (jump "wander") (Marker 0))
        (move (jump "wait") next)
    )

wander :: Code
wander = annotate "wander" $ combine
    (move this next)
    (turn L (jump "wander"))

wait :: Code
wait = annotate "wait" $ combine
    (pickUp this this)


