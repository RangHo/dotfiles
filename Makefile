.PHONY: default init install update remove

default: install

init:
	@if command -v stow >/dev/null 2>&1; then \
		ARCH=(stow) ./require.sh; \
	fi

install:
ifdef install
	@if [ -d $(install) ]; then \
		echo "Installing $(install) configs!"; \
		if [ -f $(install)/requirements ]; then \
			echo ""; \
			echo -e "\033[0;33m=== DEPENDENCIES ===\033[0m"; \
			echo "We're gonna install some programs needed!"; \
			echo "We might ask for your password here."; \
		fi; \
		./require.sh install $(install); \
		echo ""; \
		echo -e "\033[0;33m=== STOW ===\033[0m"; \
		stow $(install) \
			-t ~ \
			--ignore="requirements" \
			--ignore=".gitignore"; \
		echo "Finished symlinking!"; \
		echo ""; \
		echo "Installation finished!"; \
	fi
endif
ifdef uninstall
	@if [ -d $(uninstall) ]; then \
		stow -t ~ -D $(uninstall); \
		./require.sh uninstall $(uninstall); \
	fi
endif

update:
	@echo "Updating from the remote repository!"
	@make remove
	@git fetch --all
	@git reset --hard origin/master
	@echo "Update completed! Please reinstall configuration files!"

remove:
	@echo "Removing configuration files..."
	@for target in *; do \
		make uninstall=${target} && echo "Uninstalled ${target}."; \
	done
