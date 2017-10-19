##########################################################################
## Trying to learn R I found this algorithm at http://norvig.com         #
## and thought it would be a nice learning experience porting it to R    #
## TODO: add comments                                                    #
##########################################################################

library(testit)
#dict function
dict <- function(s, values) {
    dictionary <- character(0)
    if (is.list(values))
        dictionary[s] <- values
    else
        dictionary[s] <- list(values)

    return(dictionary)
}

#cross function
cross <- function(A, B) {
    product <- character(0)
    for (a in A) {
        for (b in B) {
            product <- c(product, paste(a, as.character(b), sep = ""))
        }
    }
    return(as.vector(product))
}

digits <<- unlist(strsplit(as.character('123456789'), ""))
rows <<- unlist(strsplit('ABCDEFGHI', ""))
cols <<- digits
squares <<- cross(rows, cols)
values <<- vector()
unitlist <- list()
for (c in cols)
    unitlist <- append(unitlist, list(cross(rows, c)))
for (r in rows)
    unitlist <- append(unitlist, list(cross(r, cols)))


rGroups <- c('ABC', 'DEF', 'GHI')
cGroups <- c('123', '456', '789')

for (i in 1:3)
    for (j in 1:3)
        unitlist <-
            append(
   unitlist,
     list(
cross(unlist(strsplit(rGroups[i], "")), unlist(strsplit(as.character(cGroups[j]), "")))
    ))

length(unitlist)

units_ <<- vector()
for (i in 1:length(squares)) {
    tmp <- vector()
    for (u in 1:length(unitlist)) {
        if (squares[i] %in% unlist(unitlist[u]))
            tmp <- append(tmp, unitlist[u])
        }
    units_[squares[i]] <- list(tmp)
}
length(units_)


peers <<- vector()
for (s in 1:length(squares)) {
    tmp <- unique(c(unlist(units_[s][[1]][1]), unlist(units_[s][[1]][2]), unlist(units_[s][[1]][3])))
    tmp <- tmp[-which(tmp %in% squares[s])]
    peers[squares[s]] <- list(tmp)
}
length(peers)

grid_values <- function(grid) {
    chars <- vector()
    gridAsVector = unlist(strsplit(as.character(grid), ""))
    for (c in gridAsVector) {
        chars <- c(chars, c)
    }
    assert("81 units_ needed to form a sodoku", length(chars) == 81)
    gridValues = vector()

    #s as index-counter for convenience
    for (s in 1:length(squares)) {
        gridValues <- c(gridValues, dict(squares[s], chars[s]))
    }
    return(gridValues)
}

parse_grid <- function(grid) {
    #Convert grid to a dict of possible values, {square: digits}, or
    #  return False if a contradiction is detected.
    values <<- dict(squares[1:length(squares)], digits)
    gridValues = grid_values(grid)

    #s as index-counter for convenience, need accessor and value A1->value(0)
    for (s in 1:length(gridValues)) {
        if (gridValues[s] %in% digits && !assign(names(gridValues[s]), gridValues[s][[1]]))

            return(FALSE)
        }

    return(values)
}

assign <- function(s, d) {
    other_values <- vector()
    if (d %in% unlist(values[s])) {
        other_values <- values[s][[1]][-which(unlist(values[s]) %in% d)]
    }

    ##tmp <- vector()
    for (d2 in other_values)
        eliminate(s, d2)
    ##tmp <- c(tmp, eliminate(s, d2))

    ##if (all(unlist(tmp[1:length(tmp)])>0)){
    if (all(unlist(values[1:length(values)]) > 0)) {
        return(TRUE)
    }
    return(FALSE)
}

eliminate <- function(s, d) {
    
    #if (!all((d %in% unlist(values[s]))==TRUE)) {
    if (!(d %in% unlist(values[s]))) {
        return(TRUE)
    }


    values[s] <<- list(values[s][[1]][-which(unlist(values[s]) %in% d)])

    ## (1) If a square s is reduced to one value d2, then eliminate d2 from the peers.
    if (length(values[s][[1]]) == 0) {
        return(FALSE) ## Contradiction: removed last value
    }
    else if (length(values[s][[1]]) == 1) {
        d2 <- values[s][[1]]

        ##tmp <- vector()
        for (s2 in peers[s][[1]]) {
            eliminate(s2, d2)
            ##tmp <- c(tmp, eliminate(s2, d2))
        }

        ##if (!all(unlist(tmp[1:length(tmp)])>0)){
        if (!all(unlist(values[1:length(values)]) > 0)) {
            return(FALSE)
        }
    }

    for (u in units_[s][[1]]) {
        dplaces <- vector()
        for (s1 in u) {
            #if (all((d %in% unlist(values[s1]))==TRUE)) {
            if (d %in% unlist(values[s1])) {
                dplaces <- c(dplaces, s1)
            }
        }

        if (length(dplaces) == 0) {
            return(FALSE) ## Contradiction: no place for this value
        }
        else if (length(dplaces) == 1) {
            #d can only be in one place in unit; assign it there
            #arrays in R start with 1
            if (!assign(dplaces[1], d)) {
                return(FALSE)
            }
        }
    }

    return(unlist(values))
}

display <- function() {
    for (r in rows) {
        line <- ""
        for (c in cols) {
            s <- paste(r, c, sep = "")
            line <- paste(line, values[s][[1]], sep = ifelse(c == 4 || c == 7, "|", ""))
        }
        print(line)
        if (r == "C" || r == "F") cat(rep(" ", 11), "\n", sep = "")
        }
}





search <- function(parsed) {
    if (!is.vector(parsed)) ##FALSE boolean 
        return(FALSE) ## Failed earlier

    if (all(lapply(values, function(x) length(x) == 1) == TRUE))
        return(TRUE)

    ## Chose the unfilled square s with the fewest possibilities
    #### (couldnt make it work :/)min( sapply (squares, function(s)  ifelse(length(values[s][[1]])>1, values[s][[1]], "Z")))

    n <- 99 #
    s <- ""
    for (s1 in squares) {
        if (length(values[s1][[1]]) > 1) {
            if (length(values[s1][[1]]) < n) {
                n <- length(values[s1])
                s <- s1
            }
        }
    }

    if (s == "") return(FALSE)

    #return some(search(assign(values.copy(), s, d)) for d in values[s])

    valuesCpy <- values ## values.copy()

    yay <- FALSE ##wt! variable
    for (d in values[s][[1]]) {
        values <<- valuesCpy
        if (assign(s, d)) {
            yay <- some(search(values))
            if (yay) break;
            }

    }

    return(yay)
}

some <- function(seq) {
    for (e in seq)
        if (e) return(TRUE)

    return(FALSE)
}
#############################
# testing implementation ...
#############################
grid <- '4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......'
parsed <- parse_grid(grid)
search(values)
display()
