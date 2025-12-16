#lang racket

(define ASCII-CHARS " .:-=+*#%@")
(define WIDTH 80)
(define HEIGHT 40)

(define (download-video url)
  (displayln "Downloading video...")
  (system (format "yt-dlp -f worst -o badapple.mp4 '~a'" url)))

(define (rgb-to-ascii r g b)
  (define brightness (quotient (+ r g b) 3))
  (define index (quotient (* brightness (- (string-length ASCII-CHARS) 1)) 255))
  (string-ref ASCII-CHARS index))

(define (extract-and-display-frame time)
  (define cmd 
    (format "ffmpeg -ss ~a -i badapple.mp4 -vframes 1 -vf scale=~a:~a -f rawvideo -pix_fmt rgb24 - 2>/dev/null"
            time WIDTH HEIGHT))
  
  (define-values (proc stdout stdin stderr)
    (subprocess #f #f #f "/bin/sh" "-c" cmd))
  
  (define pixels (port->bytes stdout))
  (close-input-port stdout)
  (subprocess-wait proc)
  
  (when (> (bytes-length pixels) 0)
    (display "\033[2J\033[H")
    
    (for ([y (in-range HEIGHT)])
      (for ([x (in-range WIDTH)])
        (define idx (* (+ (* y WIDTH) x) 3))
        (when (< (+ idx 2) (bytes-length pixels))
          (define r (bytes-ref pixels idx))
          (define g (bytes-ref pixels (+ idx 1)))
          (define b (bytes-ref pixels (+ idx 2)))
          (display (rgb-to-ascii r g b))))
      (newline))
    (flush-output)))

(define url 
  (if (> (vector-length (current-command-line-arguments)) 0)
      (vector-ref (current-command-line-arguments) 0)
      "https://youtu.be/FtutLA63Cp8"))

(download-video url)

(define fps 10.0)
(define duration 30.0)

(let loop ([time 0.0])
  (when (< time duration)
    (extract-and-display-frame time)
    (sleep (/ 1.0 fps))
    (loop (+ time (/ 1.0 fps)))))
