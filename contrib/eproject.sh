### eproject.sh --- eproject shell helper functions

# Copyright (C) 2012 Takafumi Arakaki

# Author: Takafumi Arakaki <aka.tkf at gmail.com>
# Keywords: eproject

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

### Commentary:

# To use this file, add the following line in your shell setting:
#   source PATH/TO/eproject.sh

### Code:

# Go to currently active project root in Emacs
cdp() {
    local EMACS_CWP=$(emacsclient -a false -e \
        "(eproject-current-working-directory)" \
        | sed 's/^"\(.*\)"$/\1/')
    if [ -d "$EMACS_CWP" ]; then
        cd "$EMACS_CWP"
    else
        return 1
    fi
}
