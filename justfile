# Show available commands
default:
    @just --list

# Sync Doom configuration and packages
sync:
    doom sync

# Run Doom doctor to check for issues
doctor:
    doom doctor

# Upgrade Doom Emacs and packages
upgrade:
    doom upgrade

# Clean up old packages and build artifacts
# clean:
#     doom clean

# Purge and reinstall everything (nuclear option)
purge:
    doom gc
    doom install
    doom sync

# Open Doom config in Emacs
config:
    emacs ~/.config/doom/config.el

# Reload Doom config (if Emacs is running)
reload:
    doom reload

# Run Doom with debug info
debug:
    doom run --debug

# Check Doom version and info
info:
    doom info
