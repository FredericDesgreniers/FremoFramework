function loginUsingGoogle() {
    window.location = "https://accounts.google.com/o/oauth2/v2/auth?client_id=90116680255-fqncl0rmpj9bimqcvq594lmvo3nvls60.apps.googleusercontent.com&redirect_uri="+window.location.protocol+"//"+window.location.hostname+"/login&response_type=code&scope=profile&access_type=offline"
}