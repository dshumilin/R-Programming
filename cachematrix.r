## Создаём матрицу для инверсии
makeCacheMatrix <- function( m = matrix() ) {

	## Параметры инверсии
    i <- NULL

    ## Метод для задания матрицы
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Выдаём матрицу
    get <- function() {    	m
    }

    ## Задаём обратную матрицу
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Выдаём обратную матрицу
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Возвращаем список методов
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {

    m <- x$getInverse()

    ## Если уже рассчитана инверсия, то выдаём её
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Выдаём матрицу
    data <- x$get()

    ## Высчитываем инверсию
    m <- solve(data) %*% data

    ## Задаём инверсию объекту
    x$setInverse(m)

    m
}
