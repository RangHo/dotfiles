# Create profile directory and delete the .ini file
# Or firefox will create a bunch of generated files here

mkdir --parents ~/.mozilla/firefox/RangHo
if [ -e ~/.mozilla/firefox/profiles.ini ]; then
    rm -f ~/.mozilla/firefox/profiles.ini
fi
