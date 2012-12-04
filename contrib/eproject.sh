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

cdp(){
    # Go to currently active project root in Emacs
    EMACS_CWP=$(emacsclient -e "
  (let ((current-buffer
         (nth 1 (assoc 'buffer-list
                       (nth 1 (nth 1 (current-frame-configuration)))))))
    (or (ignore-errors (eproject-root current-buffer))
        (with-current-buffer current-buffer
          (let ((filename (buffer-file-name)))
            (if filename
                (file-name-directory filename)
              default-directory)))))
    " | sed 's/^"\(.*\)"$/\1/')

    echo "chdir to $EMACS_CWP"
    cd "$EMACS_CWP"
}
