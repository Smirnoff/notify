function parameterized_url(method) {
  return $('#base_url').val() + method +
    '?email=' + encodeURIComponent($('#email').val()) + '&' +
    'access-token=' + encodeURIComponent($('#access_token').val());
}

function parameterized_streaming_url(method) {
  return $('#base_streaming_url').val() + method +
    '?email=' + encodeURIComponent($('#email').val()) + '&' +
    'access-token=' + encodeURIComponent($('#access_token').val());
}

var listening_socket;

window.onload = function() {
  $('#login').click(function() {
    $.post(
      $('#base_url').val() + '/login',
      JSON.stringify({ email: $('#email').val(),
                       password: $('#password').val() }),
      function(data) {
        var parsed = JSON.parse(data);
        console.log(parsed.message);
        $('#access_token').val(parsed['access-token']);
      });
  });
  $('#logout').click(function() {
    $.post(
      parameterized_url('/logout'),
      '',
      function(data) {
        var parsed = JSON.parse(data);
        console.log(parsed.message);
        $('#access_token').val('');
      }
    );
  });
  $('#is_logged_in').click(function() {
    $.get(
      paramterized_url('/is-logged-in'),
      '',
      function(data) {
        alert((JSON.parse(data)) ? "Logged in" : "Not logged in");
      });
  });
  $('#issue_reset_code').click(function() {
    $.post(
      $('#base_url').val() + '/issue-reset-code',
      JSON.stringify({email: $('#email').val()}),
      function(data) {
        var parsed = JSON.parse(data);
        console.log(parsed.message);
      }
    );
  });
  $('#reset_password').click(function() {
    $.post(
      $('#base_url').val() + '/apply-reset-code',
      JSON.stringify({email: $('#email').val(),
                      "reset-code": $('#reset_code').val(),
                      password: $('#password').val()}),
      function(data) {
        var parsed = JSON.parse(data);
        console.log(parsed.message);
        $('#reset_code').val('');
      }
    );
  });
  $('#get_user').click(function() {
    $.get(
      parameterized_url('/current-api-user'),
      '',
      function(data) {
        var parsed = JSON.parse(data);
        $('#user_info').val(JSON.stringify(parsed, null, 2));
      });
  });
  $('#set_user').click(function() {
    $.post(
      parameterized_url('/current-api-user'),
      $('#user_info').val(),
      function(data) {
        var parsed = JSON.parse(data);
        console.log("debugx " + data);
      });
  });

  $('#connect_disconnect').click(function() {
    if (listening_socket) {
      $('#connect_disconnect').val('Connect');
      listening_socket.close();
      listening_socket = false;
    } else {
      #('#connect_disconnect').val('Disconnect');
      listening_socket = new WebSocket(
        parameterized_streaming_url('/notifications'));
      listening_socket.onmessage = function(e) {
        var parsed = JSON.parse(e.data);
        $('#events').val(JSON.stringify(parsed, null, 2));
        $('#delete_event').click(function() {
          listening_socket.send(JSON.stringify(
            {type: "remove_change", change_id: parsed.id}));
          $('#events').val('');
        });
      };
    }
  });
};
