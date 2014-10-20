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
    (guardEntrance RightAhead)
    (guardEntrance LeftAhead)
    (guardEntrance' LeftAhead)
    (guardEntrance' RightAhead)
    (guardEntrance' Ahead)

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
        (mark 0 next)
        (sense LeftAhead (call "guardEntrance'" [mkParam LeftAhead]) this Friend)
    )
    (annotate "step2" $ combine
        (sense Ahead (jump "step4") next Home)
        (sense RightAhead next (jump "step3") (Marker 0))
        (move (call "guardEntrance" [mkParam RightAhead]) next)
    )
    (annotate "step3" $ combine
        (sense LeftAhead next (jump "wander") (Marker 0))
        (move (call "guardEntrance" [mkParam LeftAhead]) next)     
    )
    (annotate "step4" $ combine
        (pickUp next next)
        (pickUp next next)
        (pickUp next next) -- Wait to make sure the second marker has been droppd
        (sense RightAhead next (jump "step5") (Marker 0))
        (move (call "guardEntrance'" [mkParam Ahead]) next)
    )
    (annotate "step5" $ combine
        (sense LeftAhead next (jump "wander") (Marker 0))
        (move (call "guardEntrance'" [mkParam Ahead]) next)
    )

guardEntrance :: SenseDir -> Code
guardEntrance s = define "guardEntrance" [mkParam s] $ combine
    (sense s next this Foe)
    (move next this)
    (turn L next)
    (turn L next)
    (turn L next)
    (move next this)
    (turn L next)
    (turn L next)
    (turn L (call "guardEntrance" [mkParam s]))

guardEntrance' :: SenseDir -> Code
guardEntrance' s = define "guardEntrance'" [mkParam s] $ combine
    (sense s this next Friend)
    (move next this)
    (turn L next)
    (turn L next)
    (turn L next)
    (move next this)
    (turn L next)
    (turn L next)
    (turn L (call "guardEntrance'" [mkParam s]))

wander :: Code
wander = annotate "wander" $ combine
    (move this next)
    (turn L (jump "wander"))

wait :: Code
wait = annotate "wait" $ combine
    (pickUp this this)


