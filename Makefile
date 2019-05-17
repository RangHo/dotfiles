.PHONY: install update remove

install:
ifdef install
	@if [ -d $(install) ]; then \
		echo "Installing $(install) configs!"; \
		if [ -f $(install)/requirements ]; then \
			echo ""; \
			echo -e "\033[0;33m=== DEPENDENCIES ===\033[0m"; \
			echo "We're gonna install some programs needed!"; \
			echo "We might ask for your password here."; \
			./require.sh $(install)/requirements; \
		fi; \
		echo ""; \
		echo -e "\033[0;33m=== STOW ===\033[0m"; \
		stow -t ~ \
			--ignore="requirements" \
			$(install); \
		echo "Finished symlinking!"; \
		echo ""; \
		echo "Installation finished!"; \
	fi
endif
ifdef uninstall
	@if [ -d $(uninstall) ]; then stow -t ~ -D $(uninstall); fi
endif

update:
	@echo "Updating from the remote repository!"
	@for target in *; do \
		if [ -d $target ]; then \
			make uninstall=$target && echo "Uninstalled $target."; \
		fi; \
	done
	@git fetch --all
	@git reset --hard origin/master
	@echo "Update completed! Please reinstall configuration files!"

remove:
	@echo "Removing configuration files..."
	@for target in *; do \
		make uninstall=$target && echo "Uninstalled $target."; \
	done
