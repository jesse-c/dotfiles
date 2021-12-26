-- Currently playing song ------------------------------------------------------

local spotifyMenu = hs.menubar.new()

local spotifyLogger = hs.logger.new('spotifyMenu','debug')
spotifyLogger.i('Initializing')

local function hideSpotifyMenu()
  if spotifyMenu:isInMenuBar() then
    spotifyMenu:removeFromMenuBar()
    spotifyMenu:setTitle("")
  end
end

local function showSpotifyMenu()
  if not spotifyMenu:isInMenuBar() then
    spotifyMenu:returnToMenuBar()
  end
end

local function updateSpotifyMenu()
  spotifyLogger.i('callback')
  if hs.spotify.isRunning() then
    if hs.spotify.getPlaybackState() == hs.spotify.state_playing then
      spotifyLogger.i('playing')
      local currentArtist = hs.spotify.getCurrentArtist()
      local currentTrack = hs.spotify.getCurrentTrack()
      spotifyMenu:setTitle(currentArtist .. " — " .. currentTrack)

      showSpotifyMenu()
    else
      spotifyLogger.i('not playing')
      hideSpotifyMenu()
    end
  else
    hideSpotifyMenu()
  end
end

-- DISABLED hs.timer.doEvery(1, updateSpotifyMenu)

