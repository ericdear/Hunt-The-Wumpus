module wumpusIndices
    implicit none

contains
!
!   WAKE WUMPUS
!
    subroutine wakeWumpus( wumpusRoom, playerRoom, gameOver, rooms)
        implicit none
        real :: r
        integer :: wumpusRoom, playerRoom
        logical :: gameOver
        integer, dimension(20,4) :: rooms

        ! move 75% of the time
        call random_number(r)
        wumpusRoom = rooms(wumpusRoom, nint(r*3) + 1)
        
        ! check if wumpus got you
        if (wumpusRoom == playerRoom) then
            write(*, *) "TSK TSK TSK - WUMPUS GOT YOU!"
            write(*, *) "HA HA HA - YOU LOSE!"
            gameOver = .TRUE.
        end if

        return
    end subroutine wakeWumpus

!
!   CHECK FOR WUMPUS OR HAZARDS
!
    subroutine checkHazards(playerRoom, wumpusRoom, gameOver, rooms, bat1Room, bat2Room, pit1Room, pit2Room)
        integer, dimension(20,4) :: rooms
        logical :: gameOver
        integer :: playerRoom, wumpusRoom, bat1Room, bat2Room, pit1Room, pit2Room

        call checkWumpus(playerRoom, wumpusRoom, gameOver, rooms)
        call checkBat1(playerRoom, bat1Room, wumpusRoom, gameOver, rooms, pit1Room, pit2Room, bat2Room)
        call checkBat2(playerRoom, bat2Room, wumpusRoom, gameOver, rooms, pit1Room, pit2Room, bat1Room)
        call checkPit(playerRoom, pit1Room, pit2Room, gameOver)

        return
    end subroutine checkHazards

!
!   check if player fell into pit
!
    subroutine checkPit(playerRoom, pit1Room, pit2Room, gameOver)
        integer :: playerRoom, pit1Room, pit2Room
        logical :: gameOver

        if ((playerRoom == pit1Room).or.(playerRoom == pit2Room)) then
            write(*, *) "YYYIIIIEEEE . . . FELL INTO PIT"
            write(*, *) "HA HA HA - YOU LOSE!"
            gameOver = .TRUE.
        end if

        return
    end subroutine checkPit

!
!   check if you ran into bat1
!
    subroutine checkBat1(playerRoom, bat1Room, wumpusRoom, gameOver, rooms, pit1Room, pit2Room, bat2Room)
        integer :: playerRoom, bat1Room, wumpusRoom, pit1Room, pit2Room, bat2Room
        integer, dimension(20,4) :: rooms
        real :: r
        logical :: gameOver

        !write(*, *) "checking.."
        !write(*, *) "   you are in room ", playerRoom
        !write(*, *) "bat 1 is in room ", bat1Room

        if (playerRoom == bat1Room) then
            write(*, *) "ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!"
            
            ! move player and bat to random room
            call random_number(r)
            bat1Room = nint(r*19) + 1
            playerRoom = nint(r*19) + 1
            !write(*, *) "bat 1 just took you to room ", playerRoom

            ! check pit, then wumpus, then if bat2 is there
            call checkWumpus(playerRoom, wumpusRoom, gameOver, rooms)
            call checkPit(playerRoom, pit1Room, pit2Room, gameOver)
            call checkBat2(playerRoom, bat2Room, wumpusRoom, gameOver, rooms, pit1Room, pit2Room, bat1Room)
        end if

        return
    end subroutine checkBat1

!
!   check if you ran into bat2
!
    subroutine checkBat2(playerRoom, bat2Room, wumpusRoom, gameOver, rooms, pit1Room, pit2Room, bat1Room)
        integer :: playerRoom, bat2Room, wumpusRoom, pit1Room, pit2Room, bat1Room
        integer, dimension(20,4) :: rooms
        real :: r
        logical :: gameOver

        !write(*, *) "checking.."
        !write(*, *) "   you are in room ", playerRoom
        !write(*, *) "bat 2 is in room ", bat2Room

        if (playerRoom == bat2Room) then
            write(*, *) "ZAP--SUPER BAT SNATCH! ELSEWHEREVILLE FOR YOU!"
            
            ! move player and bat to random room
            call random_number(r)
            bat2Room = nint(r*19) + 1
            playerRoom = nint(r*19) + 1
            !write(*, *) "bat 2 just took you to room ", playerRoom

            ! check pit, then wumpus, then if bat2 is there
            call checkWumpus(playerRoom, wumpusRoom, gameOver, rooms)
            call checkPit(playerRoom, pit1Room, pit2Room, gameOver)
            call checkBat1(playerRoom, bat1Room, wumpusRoom, gameOver, rooms, pit1Room, pit2Room, bat2Room)
        end if

        return
    end subroutine checkBat2


!
!   check if you woke wumpus
!
    subroutine checkWumpus(playerRoom, wumpusRoom, gameOver, rooms)
        integer, dimension(20,4) :: rooms
        logical :: gameOver
        integer :: playerRoom, wumpusRoom

        if (playerRoom == wumpusRoom) then
            write(*, *) "... OOPS! BUMPED A WUMPUS!"
            call wakeWumpus(wumpusRoom, playerRoom, gameOver, rooms)
        end if

        return
    end subroutine checkWumpus


!
!   spawn everything
!   wumpus, bats, pits, and player
!
    subroutine spawn(wumpusRoom, bat1Room, bat2Room, pit1Room, pit2Room, playerRoom, &
        wumpusOriginal, bat1Original, bat2Original, playerOriginal)
        real :: r
        integer :: wumpusRoom, bat1Room, bat2Room, pit1Room, pit2Room, playerRoom, iRandom, wumpusOriginal, &
        bat1Original, bat2Original, playerOriginal

        ! set wumpus
        call random_number(r)
        wumpusRoom = nint(r*19) + 1
        wumpusOriginal = wumpusRoom

        ! set bat 1
        call random_number(r)
        bat1Room = nint(r*19) + 1
        bat1Original = bat1Room

        ! set bat 2
        call random_number(r)
        bat2Room = nint(r*19) + 1
        bat2Original = bat2Room

        ! set pit 1
        call random_number(r)
        pit1Room = nint(r*19) + 1

        ! set pit 2 (pits cant be in the same room as pit fills whole room)
        ! if the room already has a pit in it, call random number again,
        ! until it gets a room with no pit, and add a pit
        call random_number(r)
        do while (nint(r*19) == pit1Room)
            call random_number(r)
        end do
        pit2Room = nint(r*19) + 1

        ! set player location. Set in room with no wumpus and no hazards
        call random_number(r)
        iRandom = nint(r*19) + 1
        do while ((wumpusRoom == iRandom).or.(bat1Room == iRandom).or.(bat2Room == iRandom)&
            .or.(pit1Room == iRandom).or.(pit2Room == iRandom))
            call random_number(r)
            iRandom = nint(r*19) + 1
        end do
        playerRoom = iRandom
        playerOriginal = playerRoom

        return
    end subroutine spawn


!
!   game instructions
!
    subroutine instructions()
        write(*, *) "WELCOME TO 'HUNT THE WUMPUS'"
        write(*, *) "   THE WUMPUS LIVES IN A CAVE OF 20 ROOMS. EACH ROOM"
        write(*, *) "HAS 3 TUNNELS LEADING TO OTHER ROOMS. (LOOK AT A"
        write(*, *) "DODECAHEDRON TO SEE HOW THIS WORKS-IF YOU DON'T KNOW"
        write(*, *) "WHAT A DODECAHEDRON IS, ASK SOMEONE"
        write(*, *) ""
        write(*, *) "   HAZARDS:"
        write(*, *) "BOTTOMLESS PITS - TWO ROOMS HAVE BOTTOMLESS PITS IN THEM"
        write(*, *) "   IF YOU GO THERE, YOU WILL FALL INTO THE PIT (& LOSE!)"
        write(*, *) "SUPER BATS - TWO OTHER ROOMS HAVE SUPER BATS. IF YOU"
        write(*, *) "   GO THERE, A BAT GRABS YOU AND TAKES YOU TO SOME OTHER"
        write(*, *) "   ROOM AT RANDOM. (WHICH MIGHT BE TROUBLESOME)"
        write(*, *) ""
        write(*, *) "   WUMPUS:"
        write(*, *) "THE WUMPUS IS NOT BOTHERED BY THE HAZARDS (HE HAS SUCKER"
        write(*, *) "FEET AND IS TOO BIG FOR A BAT TO LIFE). USUALLY"
        write(*, *) "HE IS ASLEEP. TWO THINGS WAKE HIM UP: YOUR ENTERING"
        write(*, *) "HIS ROOM OR YOUR SHOOTING AN ARROW."
        write(*, *) "   IF THE WUMPUS WAKES, HE MOVES (P=.75) ONE ROOM"
        write(*, *) "OR STAYS STILL (P=.25). AFTER THAT, IF HE IS WHERE YOU"
        write(*, *) "ARE, HE EATS YOU UP (& YOU LOSE!)"
        write(*, *) ""
        write(*, *) "   YOU:"
        write(*, *) "EACH TURN YOU MAY MOVE OR SHOOT A CROOKED ARROW"
        write(*, *) "   MOVING: YOU CAN GO ONE ROOM (THRU ONE TUNNEL)"
        write(*, *) "   ARROWS: YOU HAVE 5 ARROWS. YOU LOSE WHEN YOU RUN OUT."
        write(*, *) "   EACH ARROW CAN GO FROM 1 TO 5 ROOMS. YOU AIM BY TELLING"
        write(*, *) "   THE COMPUTER THE ROOM #S YOU WANT THE ARROW TO GO TO."
        write(*, *) "   IF THE ARROW CAN'T GO THAT WAY(IE NO TUNNEL) IT MOVES"
        write(*, *) "   AT RANDOM TO THE NEXT ROOM."
        write(*, *) "       IF THE ARROW HITS THE WUMPUS, YOU WIN."
        write(*, *) "       IF THE ARROW HITS YOU, YOU LOSE."
        write(*, *) ""
        write(*, *) "   WARNINGS:"
        write(*, *) "       WHEN YOU ARE ONE ROOM AWAY FROM WUMPUS OR HAZARD,"
        write(*, *) "   THE COMPUTER SAYS:"
        write(*, *) "WUMPUS - 'I SMELL A WUMPUS'"
        write(*, *) "BAT    - 'BATS NEARBY'"
        write(*, *) "PIT    - 'I FEEL A DRAFT'"
        write(*, *) ""

        return
    end subroutine instructions


!
!   print any hazards nearby (hazards in each exiting room)
!
    subroutine nearbyHazards(playerRoom, wumpusRoom, bat1Room, bat2Room, pit1Room, pit2Room, rooms)
        integer :: playerRoom, wumpusRoom, bat1Room, bat2Room, pit1Room, pit2Room, i
        integer, dimension(20,4) :: rooms

        do i = 2, 4
            ! check for wumpus, bats, and pits
            if (rooms(playerRoom, i) == wumpusRoom) write(*, *) "I SMELL A WUMPUS!"
            if ((rooms(playerRoom, i) == bat1Room).or.(rooms(playerRoom, i) == bat2Room)) write(*, *) "BATS NEARBY!"
            if ((rooms(playerRoom, i) == pit1Room).or.(rooms(playerRoom, i) == pit2Room)) write(*, *) "I FEEL A DRAFT!"
        end do

        return
    end subroutine nearbyHazards


!
!   print current room
!
    subroutine printRoom(playerRoom, rooms)
        integer :: playerRoom
        integer, dimension(20,4) :: rooms

        write(*, '(a, i3)') "YOU ARE IN ROOM ", playerRoom
        write(*, '(a, i2, a, i2, a, i2)') "TUNNELS LEAD TO ROOMS ", rooms(playerRoom, 2), &
        ", ", rooms(playerRoom, 3), ", and ", rooms(playerRoom, 4)
        write(*, *) ""
        write(*, '(a)') "\         \   |         |   /         /"
        write(*, '(a, i2, a, i2, a, i2, a)') " \   ", rooms(playerRoom, 2), &
        "    \  |   ", rooms(playerRoom, 3), "    |  /   ", rooms(playerRoom, 4), "    /"
        write(*, *) " \         \ |         | /         /"
        write(*, *) "  \         \|         |/         /"
        write(*, *) "   \                             /"
        write(*, *) "    \                           /"
        write(*, *) "     \            o            /"
        write(*, *) "      \          /|\          /"
        write(*, *) "       \         / \         /"

        return
    end subroutine printRoom


!
!   ask user what to do, move or shoot
!
    subroutine ask(action)
        character :: action

        write(*, *) "SHOOT OR MOVE (S-M)"
        read(*, *) action
        do while ((action /= 'M').and.(action /= 'S'))
            write(*, *) "Please type 'S' or 'M'"
            read(*, *) action
        end do

        return
    end subroutine ask


!
!   if the arrow missed, check if out of arrows, then wake the wumpus
!
    subroutine missedArrow(gameOver, arrowsLeft, wumpusRoom, playerRoom, rooms)
        integer, dimension(20,4) :: rooms
        logical :: gameOver
        integer :: arrowsLeft, wumpusRoom, playerRoom

        if (gameOver.eqv. .FALSE.) then
            write(*, *) "MISSED"
            if (arrowsLeft == 0) then
                write(*, *) "OUT OF ARROWS"
                write(*, *) "HA HA HA - YOU LOSE!"
                gameOver = .TRUE.
            
            ! if arrow missed, wumpus moves 1 spot 75% or stays still 25%
            ! if random number is 0 it moves to same room (not moving) 1/4 times
            else
                call wakeWumpus(wumpusRoom, playerRoom, gameOver, rooms)
                !write(*, *) "wumpus moved to ", wumpusRoom
            end if
        end if

        return
    end subroutine missedArrow


!
!   ask the user where to move, and move the player
!
    subroutine movePlayer(playerRoom, rooms)
        integer :: iostat, playerRoom, nextRoom
        integer, dimension(20,4) :: rooms

        ! ask user wherwe to go, check if its valid
        write(*, *) "WHERE TO?"
        read(*, '(i20)', iostat = iostat) nextRoom
        ! ask the user again, if the input is invalid, or if its not any of the exiting rooms
        do while ((iostat /= 0).or.((nextRoom /= rooms(playerRoom, 2)).and.(nextRoom /= rooms(playerRoom, 3))&
            .and.(nextRoom /= rooms(playerRoom, 4))))
            write(*, *) "NOT POSSIBLE, SCROLL UP TO SEE DIAGRAM"
            write(*, *) "WHERE TO?"
            read(*, '(i20)', iostat = iostat) nextRoom
        end do

        ! move player
        playerRoom = nextRoom

        return
    end subroutine movePlayer


!
!   ask the player where to shoot the arrow, 
!   if they say a room that there is no direct tunnel there, a random room is chosen
!
    subroutine aimArrow(arrowNextRoom, arrowPreviousRoom, arrowCurrentRoom, rooms)
        integer :: arrowNextRoom, arrowPreviousRoom, arrowCurrentRoom, iostat
        real :: r
        integer, dimension(20,4) :: rooms

        write(*, *) "ROOM #?"
        read(*, '(i20)', iostat = iostat) arrowNextRoom
        do while ((iostat /= 0).or.(arrowNextRoom <= 0).or.(arrowNextRoom > 20).or.(arrowNextRoom == arrowPreviousRoom)&
            .or.(arrowNextRoom == arrowCurrentRoom))
            write(*, *) "ARROW AREN'T THAT CROOKED - TRY ANOTHER ROOM"
            read(*, '(i20)', iostat = iostat) arrowNextRoom
        end do

        ! if no tunnel, move to random room
        if ((arrowNextRoom /= rooms(arrowCurrentRoom, 2)).and.(arrowNextRoom /= rooms(arrowCurrentRoom, 3))&
        .and.(arrowNextRoom /= rooms(arrowCurrentRoom, 4))) then
            ! set next room = to a random room that is not the previous room
            call random_number(r)
            do while (rooms(arrowCurrentRoom, nint(r*2) + 2) == arrowPreviousRoom)
                call random_number(r)
            end do
            arrowNextRoom = rooms(arrowCurrentRoom, nint(r*2) + 2)
        end if

        return
    end subroutine aimArrow


!
!   ask the player how far to shoot the arrow
!
    subroutine arrowSpeed(arrowRooms)
        integer :: arrowRooms, iostat

        write(*, *) "NO. OF ROOMS? (1-5)"
        read(*, '(i20)', iostat = iostat) arrowRooms
        do while ((iostat /= 0).or.(arrowRooms <= 0).or.(arrowRooms > 5))
            write(*, *) "Please type a number (1-5)"
            read(*, '(i20)', iostat = iostat) arrowRooms
        end do

        return
    end subroutine arrowSpeed


end module wumpusIndices