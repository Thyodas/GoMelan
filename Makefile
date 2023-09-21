##
## EPITECH PROJECT, 2023
## makefile
## File description:
## makefile
##

BINARY_PATH	:=	$(shell stack path --local-install-root)
NAME		=	glados

all:
	@stack build
	@cp "$(BINARY_PATH)/bin/$(NAME)-exe" "./$(NAME)"

clean:
	@stack clean

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
