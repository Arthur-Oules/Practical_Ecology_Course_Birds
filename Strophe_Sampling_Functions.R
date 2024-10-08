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
silences_44k <- sapply(durations, \(x) create_audio(x, sample_rate = 44100))
names(silences_44k) <- paste0("sec_", durations)
silences_48k <- sapply(durations, \(x) create_audio(x, sample_rate = 48000))
names(silences_48k) <- paste0("sec_", durations)

rm(durations)

#______________________________________________________________________________#

# Return a concatenated audio file from an audio file list.
concat_audio <- function(audio_files_list){
        audio_concat <- audio_files_list[[1]]
        for (audio_file in audio_files_list[-1]){
                audio_concat <- bind(audio_concat, audio_file)
        }
        audio_concat
}

# Return an audio file that alternates segments from audio files of two lists.
# list1 et list2 have to be of same length
alternative_audio_concat <- function(list1, list2){
        n <- length(list1) # == length(list2)
        if (n != length(list2)) {
                stop("Error! The size of the lists is not identical.")
        }
        result <- vector("list", 2*n)
        result[seq(1, length(result), by = 2)] <- list1
        result[seq(2, length(result), by = 2)] <- list2
        concat_audio(result)
}

#______________________________________________________________________________#

# Take a path through a given individual (bird)
# Returns the randomly mixed list of complete strophes (or gazoulli or song)
# for a given individual.
individual_mix_audio_list <- function(path) {
        strophes <- sample(list.files(path))
        n <- length(strophes)
        result <- list()
        
        for (i in 1:n){
                file_name <- tools::file_path_sans_ext(strophes[i])
                result[[file_name]] <- readWave(here(path, strophes[i]))
        }
        result
}

#______________________________________________________________________________#

# Take the list of audios of a given bird and create the final random audio file
add_silences <- function(list_audio,
                         list_silences = silences_44k,
                         time_max      = 90) {
        n <- length(list_audio)
        list_silences <- sample(list_silences, n, replace = TRUE)
        final_audio <- alternative_audio_concat(list_audio, list_silences)
        length_max <- final_audio@samp.rate * time_max
        
        # We return an audio file of a maximum of 1 minute and 30 seconds.
        # If not long enough, we start over from the beginning.
        if (length(final_audio) > length_max) {
                final_audio <- final_audio[1:length_max]
        } else {
                final_audio <- concat_audio(list(final_audio, final_audio))
                final_audio <- final_audio[1:length_max]
        }
        # We include 10 seconds of silence at the end of the audio so that,
        # in practice, the person using the phone can listen to the full
        # 90 seconds without the next audio starting immediately.
        sampling_rate <- final_audio@samp.rate
        final_silence <- create_audio(10, sampling_rate)
        final_audio <- concat_audio(list(final_audio, final_silence))
        return(final_audio)
}

#______________________________________________________________________________#

# Create a folder if it doesn't already exist
create_folder <- function(path) {
        if (!dir.exists(path)) {dir.create(path, recursive = TRUE)}
}
