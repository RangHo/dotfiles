/**
 * Firefox user preferences
 * 
 * What it does:
 *    - Allow OS to draw in titlebar (for prettier Firefox on Linux)
 *    - Set ALL fonts to Noto fonts because fontconfig is an asshole
 *        (obviously requires Noto fonts to be installed first)
 *    - Makes fontconfig matching bearable by making font
 *        substitution more generic
 */

user_pref("browser.tabs.drawInTitlebar", true);
user_pref("browser.display.use_system_colors", false);
user_pref("font.name.monospace.ar", "Noto Sans Mono");
user_pref("font.name.monospace.el", "Noto Sans Mono");
user_pref("font.name.monospace.he", "Noto Sans Mono");
user_pref("font.name.monospace.ja", "Noto Sans Mono CJK JP");
user_pref("font.name.monospace.ko", "Noto Sans Mono CJK KR");
user_pref("font.name.monospace.th", "Noto Sans Mono");
user_pref("font.name.monospace.x-armn", "Noto Sans Mono");
user_pref("font.name.monospace.x-beng", "Noto Sans Mono");
user_pref("font.name.monospace.x-cans", "Noto Sans Mono");
user_pref("font.name.monospace.x-cyrillic", "Noto Sans Mono");
user_pref("font.name.monospace.x-devanagari", "Noto Sans Mono");
user_pref("font.name.monospace.x-ethi", "Noto Sans Mono");
user_pref("font.name.monospace.x-geor", "Noto Sans Mono");
user_pref("font.name.monospace.x-gujr", "Noto Sans Mono");
user_pref("font.name.monospace.x-guru", "Noto Sans Mono");
user_pref("font.name.monospace.x-khmr", "Noto Sans Mono");
user_pref("font.name.monospace.x-knda", "Noto Sans Mono");
user_pref("font.name.monospace.x-mlym", "Noto Sans Mono");
user_pref("font.name.monospace.x-orya", "Noto Sans Mono");
user_pref("font.name.monospace.x-sinh", "Noto Sans Mono");
user_pref("font.name.monospace.x-tamil", "Noto Sans Mono");
user_pref("font.name.monospace.x-telu", "Noto Sans Mono");
user_pref("font.name.monospace.x-tibt", "Noto Sans Mono");
user_pref("font.name.monospace.x-unicode", "Noto Sans Mono");
user_pref("font.name.monospace.x-western", "Noto Sans Mono");
user_pref("font.name.monospace.zh-CN", "Noto Sans Mono CJK SC");
user_pref("font.name.monospace.zh-HK", "Noto Sans Mono CJK TC");
user_pref("font.name.monospace.zh-TW", "Noto Sans Mono CJK TC");
user_pref("font.name.sans-serif.ar", "Noto Sans Arabic");
user_pref("font.name.sans-serif.el", "Noto Sans");
user_pref("font.name.sans-serif.he", "Noto Sans Hebrew");
user_pref("font.name.sans-serif.ja", "Noto Sans CJK JP");
user_pref("font.name.sans-serif.ko", "Noto Sans CJK KR");
user_pref("font.name.sans-serif.th", "Noto Sans Thai");
user_pref("font.name.sans-serif.x-armn", "Noto Sans Armenian");
user_pref("font.name.sans-serif.x-beng", "Noto Sans Bengali");
user_pref("font.name.sans-serif.x-cans", "Noto Sans Canadian Aboriginal");
user_pref("font.name.sans-serif.x-cyrillic", "Noto Sans");
user_pref("font.name.sans-serif.x-devanagari", "Noto Sans Devanagari");
user_pref("font.name.sans-serif.x-ethi", "Noto Sans Ethiopic");
user_pref("font.name.sans-serif.x-geor", "Noto Sans Georgian");
user_pref("font.name.sans-serif.x-gujr", "Noto Sans Gujarati");
user_pref("font.name.sans-serif.x-guru", "Noto Sans Gurmukhi");
user_pref("font.name.sans-serif.x-khmr", "Noto Sans Khmer");
user_pref("font.name.sans-serif.x-knda", "Noto Sans Kannada");
user_pref("font.name.sans-serif.x-mlym", "Noto Sans Malayalam");
user_pref("font.name.sans-serif.x-orya", "Noto Sans Oriya");
user_pref("font.name.sans-serif.x-sinh", "Noto Sans Sinhala");
user_pref("font.name.sans-serif.x-tamil", "Noto Sans Tamil");
user_pref("font.name.sans-serif.x-telu", "Noto Sans Telugu");
user_pref("font.name.sans-serif.x-tibt", "Noto Sans Tibetan");
user_pref("font.name.sans-serif.x-unicode", "Noto Sans");
user_pref("font.name.sans-serif.x-western", "Noto Sans");
user_pref("font.name.sans-serif.zh-CN", "Noto Sans CJK SC");
user_pref("font.name.sans-serif.zh-HK", "Noto Sans CJK TC");
user_pref("font.name.sans-serif.zh-TW", "Noto Sans CJK TC");
user_pref("font.name.serif.ar", "Noto Naskh Arabic");
user_pref("font.name.serif.el", "Noto Serif");
user_pref("font.name.serif.he", "Noto Serif Hebrew");
user_pref("font.name.serif.ja", "Noto Serif CJK JP");
user_pref("font.name.serif.ko", "Noto Serif CJK KR");
user_pref("font.name.serif.th", "Noto Serif Thai");
user_pref("font.name.serif.x-armn", "Noto Serif Armenian");
user_pref("font.name.serif.x-beng", "Noto Serif Bengali");
user_pref("font.name.serif.x-cans", "Noto Sans Canadian Aboriginal");
user_pref("font.name.serif.x-cyrillic", "Noto Serif");
user_pref("font.name.serif.x-devanagari", "Noto Serif Devanagari");
user_pref("font.name.serif.x-ethi", "Noto Serif Ethiopic");
user_pref("font.name.serif.x-geor", "Noto Serif Georgian");
user_pref("font.name.serif.x-gujr", "Noto Serif Gujarati");
user_pref("font.name.serif.x-guru", "Noto Serif Gurmukhi");
user_pref("font.name.serif.x-khmr", "Noto Serif Khmer");
user_pref("font.name.serif.x-knda", "Noto Serif Kannada");
user_pref("font.name.serif.x-mlym", "Noto Serif Malayalam");
user_pref("font.name.serif.x-orya", "Noto Sans Oriya");
user_pref("font.name.serif.x-sinh", "Noto Serif Sinhala");
user_pref("font.name.serif.x-tamil", "Noto Serif Tamil");
user_pref("font.name.serif.x-telu", "Noto Serif Telugu");
user_pref("font.name.serif.x-tibt", "Noto Serif Tibetan");
user_pref("font.name.serif.x-unicode", "Noto Serif");
user_pref("font.name.serif.x-western", "Noto Serif");
user_pref("font.name.serif.zh-CN", "Noto Serif CJK SC");
user_pref("font.name.serif.zh-HK", "Noto Serif CJK TC");
user_pref("font.name.serif.zh-TW", "Noto Serif CJK TC");
user_pref("gfx.font_rendering.fontconfig.max_generic_substitutions", 127);