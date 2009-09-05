## RQuantLib -- R interface to the QuantLib libraries
##
## Copyright (C) 2002 - 2009 Dirk Eddelbuettel <edd@debian.org>
##
## $Id: calendars.R 63 2009-04-10 20:24:04Z edd $
##
## This file is part of the RQuantLib library for GNU R.
## It is made available under the terms of the GNU General Public
## License, version 2, or at your option, any later version,
## incorporated herein by reference.
##
## This program is distributed in the hope that it will be
## useful, but WITHOUT ANY WARRANTY; without even the implied
## warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
## PURPOSE.  See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public
## License along with this program; if not, write to the Free
## Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
## MA 02111-1307, USA

businessDay <- function(calendar="TARGET", dates=Sys.Date()) {
    stopifnot(is.character(calendar))
    stopifnot(class(dates)=="Date")
    val <- .Call("QL_isBusinessDay",
                 list(calendar=calendar),
		 dates,
                 PACKAGE="RQuantLib")
    val <- as.logical(val[[1]])
    names(val) <- dates
    val
}
