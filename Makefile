##
## EPITECH PROJECT, 2020
## Makefile
## File description:
## 201yams compilation
##

SRC     = 201yams.hs

NAME    =	201yams

all:	$(NAME)

$(NAME):
	ghc -o $(NAME) $(SRC)

clean:
	rm -f *.o *.hi

fclean:	clean
	rm -f $(NAME)

re:	fclean all