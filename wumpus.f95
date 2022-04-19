program a4
        use wumpusIndices

        ! Array of rooms: each room number with 3 exits, possibility for wumpus, bat1, bat2, and a pit
        integer, dimension(20,4) :: rooms
        integer, dimension(20) :: exit1 = (/ 11, 1, 1, 3, 4, 4, 3, 7, 6, 5, 1, 7, 8, 9, 10, 2, 12, 17, 14, 11 /)
        integer, dimension(20) :: exit2 = (/ 3, 5, 7, 5, 2, 8, 8, 6, 10, 9, 12, 11, 14, 13, 19, 15, 13, 19, 15, 18 /)
        integer, dimension(20) :: exit3 = (/ 2, 16, 4, 6, 10, 9, 12, 13, 14, 15, 20, 17, 17, 19, 16, 20, 18, 20, 18, 16 /)

        logical :: gameOver = .FALSE.
        integer :: playerRoom
        character :: action = 'N'
        integer :: wumpusOriginal, bat1Original, bat2Original, playerOriginal
        integer :: wumpusRoom, bat1Room, bat2Room, pit1Room, pit2Room
        integer :: arrowPreviousRoom = 0, arrowRooms = 0, arrowNextRoom = 0, arrowCurrentRoom = 0, arrowsLeft = 5

        ! set up the rooms array
        do i = 1, 20
            ! set rooms and exits
            rooms(i, 1) = i
            rooms(i, 2) = exit1(i)
            rooms(i, 3) = exit2(i)
            rooms(i, 4) = exit3(i)
        end do

        ! spawn everything onto the map
        call spawn(wumpusRoom, bat1Room, bat2Room, pit1Room, pit2Room, playerRoom, wumpusOriginal, bat1Original, &
        bat2Original, playerOriginal)

        ! show the user the instructions
        call instructions()

        ! game loop
        do while (gameOver.eqv. .FALSE.)

            ! print nearby hazards
            call nearbyHazards(playerRoom, wumpusRoom, bat1Room, bat2Room, pit1Room, pit2Room, rooms)

            ! print current room
            call printRoom(playerRoom, rooms)

            ! ask user what to do
            call ask(action)

            ! if user wants to shoot
            if (action == 'S') then
                arrowsLeft = arrowsLeft - 1
                arrowPreviousRoom = 0
                arrowCurrentRoom = playerRoom

                ! select the number of rooms the arrow will go through
                call arrowSpeed(arrowRooms)

                ! ask the user what room to aim the arrow
                do i = 1, arrowRooms
                    call aimArrow(arrowNextRoom, arrowPreviousRoom, arrowCurrentRoom, rooms)

                    ! arrow shot to next room
                    arrowPreviousRoom = arrowCurrentRoom
                    arrowCurrentRoom = arrowNextRoom

                    ! check if arrow hit wumpus or player
                    if (arrowCurrentRoom == wumpusRoom) then
                        write(*, *) "AHA! YOU GOT THE WUMPUS"
                        write(*, *) "HEE HEE HEE - THE WUMPUS'LL GETCHA NEXT TIME!!"
                        gameOver = .TRUE.
                        exit
                    else if (arrowCurrentRoom == playerRoom) then
                        write(*, *) "OUCH! ARROW GOT YOU!"
                        write(*, *) "HA HA HA - YOU LOSE!"
                        gameOver = .TRUE.
                        exit
                    end if
                end do

                ! arrow missed
                call missedArrow(gameOver, arrowsLeft, wumpusRoom, playerRoom, rooms)
            
            ! if user wants to move
            else
                call movePlayer(playerRoom, rooms)

                ! check if you hit any hazards
                call checkHazards(playerRoom, wumpusRoom, gameOver, rooms, bat1Room, bat2Room, pit1Room, pit2Room)
            end if

            if (gameOver.eqv. .TRUE.) then
                write(*, *) "SAME SETUP (Y-N)?"
                read(*, *) action
                do while ((action /= 'Y').and.(action /= 'N'))
                    write(*, *) "Please type 'Y' or 'N'"
                    read(*, *) action
                end do

                ! reset arrows and game back on !
                arrowsLeft = 5
                gameOver = .FALSE.

                if (action == 'Y') then
                    ! reset player, wumpus, and bats, to original spots
                    playerRoom = playerOriginal
                    wumpusRoom = wumpusOriginal
                    bat1Room = bat1Original
                    bat2Room = bat2Original

                else
                    ! reset everything to a new random
                    call spawn(wumpusRoom, bat1Room, bat2Room, pit1Room, pit2Room, playerRoom, &
                        wumpusOriginal, bat1Original, bat2Original, playerOriginal)
                end if
            end if

        end do


end