args = commandArgs(trailingOnly=TRUE)

if(length(args) == 0){
	stop("USAGE: Rscript hw1_108703030.R input", call.=FALSE)
}else{
	#input first
	if(args[1] == "--input"){
		in_path <- args[2]
		out_path <- args[4]
	}else{
		#out first

		in_path <- args[4]
		out_path <- args[2]
	}


	if(file.exists(in_path) == FALSE){
		print("Input doesn't exist !")
	}else{
		r_in <- read.csv(in_path)

		if( is.null(r_in[['weight']]) || is.null(r_in[['height']]) ){
			print("This is a wrong data!")
		}else{      
			# w , h exist , and get the biggest num
			w <- max(r_in$weight)
			h <- max(r_in$height)

			# get the last file name
			in_file <- basename(in_path)

			# remove ".csv"
			input <- gsub(".csv","",in_file) 

			# make a data frame
			d <- data.frame(
					set = input,
					weight = round(w,2),
					height = round(h,2)
			)

			# write file
			out_file <- basename(out_path)

			if(out_path == out_file){
				# same dir
				write.csv(d,out_file ,quote = FALSE, row.names = FALSE)

			}else{
				out_path <- gsub(out_file,"",out_path)

				# make dir
				dir.create(out_path, recursive = TRUE ,showWarnings = FALSE)

				# set path
				setwd(out_path)

				write.csv(d,out_file ,quote = FALSE, row.names = FALSE)

			}

		}
	}

}


