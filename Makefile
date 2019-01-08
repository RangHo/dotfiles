.PHONY: install update remove

install:
ifdef install
	@if [ -d $(install) ]; then \
		if [ -f $(install)/init.sh ]; then \
			echo "Initialization script found. Executing..."; \
			$(install)/init.sh; \
		else \
			echo "Initialization script is not found. Skipping..."; \
		fi; \
		stow -t ~ --ignore="init\.sh" --ignore="final\.sh" $(install); \
		if [ -f $(install)/final.sh ]; then \
			echo "Finalization script found. Executing..."; \
			$(install)/final.sh; \
		else \
			echo "Finalization script is not found. Skipping..."; \
		fi; \
	fi
endif
ifdef uninstall
	@if [ -d $(uninstall) ]; then stow -t ~ -D $(uninstall); fi
endif

update:
	@for target in *; do \
		if [ -d $target ]; then \
			stow -t ~ -D $target; \
		fi; \
	done
	@git fetch --all
	@git reset --hard origin/master
	@echo "Update complete. Please re-install configuration files."

remove:
	@for target in *; do \
		if [ -d $target ]; then \
			stow -t ~ -D $target; \
		fi; \
	done
