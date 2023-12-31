##
## EPITECH PROJECT, 2023
## makefile
## File description:
## makefile
##

BINARY_PATH	:=	$(shell stack path --local-install-root)
NAME		=	glados

all:
	@stack build --allow-different-user
	@cp "$(BINARY_PATH)/bin/$(NAME)-exe" "./$(NAME)"

tests_run:
	@stack test --coverage --allow-different-user

clean:
	@stack clean --allow-different-user

fclean: clean
	@rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re

coding_style: clean
	@printf "${IWHITE}              ${BOLD}${CYAN}CODING STYLE              \
	${END}\n\n"
	@printf "${CYAN}"
	../../coding-style-checker/coding-style.sh app/ .
	@printf "${END}"

.PHONY: all, re, clean, fclean, coding_style
