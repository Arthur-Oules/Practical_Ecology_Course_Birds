# In this R file, we define all the functions that we will use to build
# the audio files for the urban ecology practical.

# library(tuneR) # Logically, already imported into the Notebook.

#______________________________________________________________________________#

# Firstly, we create the silent audio files.

# Create an audio file of a given length
create_audio <- function(duration, sample_rate = 44100, bits = 16) {
        # Create an audio data vector filled with zeros
        audio_data <- numeric(duration * sample_rate)
        # Create a wave object
        audio <- Wave(audio_data, samp.rate = sample_rate, bit = bits)
}

# Durations of silences (in seconds)
durations <- c(2.5, 3, 3.5, 4, 4.5)

# Create a list of silent audio files
silences <- sapply(durations, create_audio)
names(silences) <- paste0("sec_", durations)
big_silence <- create_audio(10)

rm(silence, durations)

#______________________________________________________________________________#

# Return a concatenated audio file from an audio file list.
concat_audio <- function(audio_files_list){
        audio_concat <- audio_files_list[[1]]
        for (audio_file in audio_files_list[-1]){
                audio_concat <- bind(audio_concat, audio_file)
        }
        return(audio_concat)
}

# Return an audio file that alternates segments from audio files of two lists.
# list1 et list2 have to be of same length
alternative_audio_concat <- function(list1, list2){
        n <- length(list1) # == length(list2)
        if (n != length(list2)){
                stop("Attention ! La taille des listes n'est pas la même.")
        }
        result <- vector("list", 2*n)
        result[seq(1, length(result), by = 2)] <- list1
        result[seq(2, length(result), by = 2)] <- list2
        result <- concat_audio(result)
        return(result)
}

#______________________________________________________________________________#

# Take a path through a given individual (bird)
# Returns the randomly mixed list of complete strophes (or gazoulli or song)
# for a given individual.
individual_mix_audio_list <- function(path){
        strophes <- list.files(path) |> sample()
        n <- length(strophes)
        result <- list()
        
        for (i in 1:n){
                file_name <- strophes[i] |> tools::file_path_sans_ext()
                result[[file_name]] <- here(path, strophes[i]) |> readWave()
        }
        return(result)
}

#______________________________________________________________________________#

# Take the list of audios of a given bird and create the final random audio file
add_silences <- function(
                list_audio, list_silences=silences, time_max = 90,
                final_silence = big_silence){
        n <- length(list_audio)
        list_silences <- sample(list_silences, n, replace=T)
        final_audio <- alternative_audio_concat(list_audio, list_silences)
        length_max <- final_audio@samp.rate * time_max
        
        # We return an audio file of a maximum of 1 minute and 30 seconds.
        # If not long enough, we start over from the beginning.
        if (length(final_audio) > length_max){
                final_audio <- final_audio[1:length_max]
        }else{
                final_audio <- concat_audio(list(final_audio, final_audio))
                final_audio <- final_audio[1:length_max]
        }
        # We include 10 seconds of silence at the end of the audio so that,
        # in practice, the person using the phone can listen to the full
        # 90 seconds without the next audio starting immediately.
        final_audio <- concat_audio(list(final_audio, final_silence))
        return(final_audio)
}

#______________________________________________________________________________#

# Create a folder if it doesn't already exist
create_folder <- function(path){
        if (!dir.exists(path)){
                dir.create(path, recursive = TRUE)
        }
}
