HTML_styles <- '
      * {
        font-family: "Gotham";
      }
      .content {
        min-height: 1000px;
      }

      body {
        color: #fff;
        background-color: #242424;
      }
      .box-header {
      background-color: #040404;
      }
      .main-sidebar .sidebar .sidebar-menu {
        position: fixed;
        width: 230px;
        height: 100%;
        background-color: #040404;
      }
      .nav-tabs-custom>.nav-tabs>li.active>a{
       background-color: #1DB954;
        color: #fff;
      }
      .nav-tabs-custom>.nav-tabs>li.active {
        border-top-color: #191414;
        }


      html {
        font-size: 16px;
        color: #FFFFFF !important;
      }
      .content-wrapper .content {
          background-color: #121212;
          margin-bottom: 100px;
          
      }
      .content{
        margin-bottom: 100px;
      }
      
      * {
          font-family: "Open Sans", sans-serif;
          letter-spacing: -0.35px;
      }

      skin-blue .main-sidebar {
        background-color: #040404;
      }
      .logo {
        background-color:  #040404 !important;
        position:fixed;
      }
      .navbar {
        background-color:  #040404 !important;
      }
      #sidebarCollapsed {
        background-color: #040404;
      }

      .skin-blue .main-sidebar .sidebar .sidebar-menu  a{
        border-radius: 5px;
        border-color: transparent;
      }


      .top-image {
        display: block;
        margin-left: auto;
        margin-right: auto;
        margin-bottom: 20px;
        width: 90%;
        box-shadow: 10px 5px 5px black;
        object-fit: cover;
        height: 162px;
        width: 162px;
      }


      .top-text {
        display: inline-block;
        margin-left: 20px;
        margin-bottom: 55px;
      }


      .box1 {
        background-color: #242424;
        border-top: 0;
        transition: 0.3s;
        min-width: 200px;
        min-height:350px;
        max-width: 320px;
        column-gap: 10px;
        margin: 20px;
        border-radius:5px;
      }
      box11 {
        background-color: #242424;
        border-top: 0;
        transition: 0.3s;
        min-width: 400px;
        min-height:800px;
        
        column-gap: 30px;
        margin: 20px;
        border-radius:5px;
      }
      .box{
      background-color: #121212;
      border-top:0;
      }
      .col-sm-6 {
      width: 20%;
      min-width: 220px;
      }
      .us-image{
        width: 250px;
        height: 250px;
        margin: 10px;
      }

      .text-fav {
        margin-left:30px;
        font-weight:bold;

      }
      .box1:hover {
        background-color: #2a2a2a
      }
      .box11:hover {
        background-color: #2a2a2a
      }
      .description-box:: hover {
        background-color: #2a2a2a
      }
      .description-box{
        background-color: #242424;
        border-top: 0;
        transition: 0.3s;
        margin: 20px;
        border-radius:5px;
      }
      .us-box{
        background-color: #242424;
        border-top: 0;
        transition: 0.3s;
        margin: 20px;
        border-radius:5px;
        min-width: 300px;
        min-height: 600px;
        
      }
      
      .pretty .state label {
        color: #b8c7ce;
        height:20px;

      }


      }
      .shiny-input-checkboxgroup label~.shiny-options-group, .shiny-input-radiogroup label~.shiny-options-group {
      margin-top:20px;

      }


  .pretty .state label {
    top:0;
    margin-bottom:10px;
    margin-top:10px;

  }
  .pretty .state label:hover {
    top:0;
    margin-bottom:10px;
    margin-top:10px;
    color: #FFFFFF;
  }



  .pretty .state label:before {
  top:10px;}
  .pretty .state label:after {
  top:10px;}



      .control-label{
      font-weight:600;
      margin-bottom: 20px;
      }


      .irs{
      width: 500px;

      }
      .suwak-panel {
        position: fixed;
        bottom: 0;
        width: 100%;
        background-color: #2a2a2a;
        height: 100px;
        left: 0;
        z-index: 1000;
      }
      

          .irs--shiny .irs-bar {
        top: 25px;
        height: 8px;
        border-top: 1px solid #1DB954;
        border-bottom: 1px solid #1DB954;
        background: #1DB954;
          }
      .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
      color: #FFFFFF;
      text-shadow: none;
      line-height:1px;
      padding: 4px;
      background-color:#121212;
      border-radius: 3px;
      font-size: 11px;

      }
      .icons-box{
        width: 200px;
        margin-top:30px;
        margin-bottom:0;
        margin-left: auto;
        margin-right: auto;
        display: flex;
        justify-content: center;
        flex-direction: row;
      }
      
      .suwak-container{
        display: flex;
        justify-content: center;
        flex-direction: row;
        
        position: fixed;
        bottom: 0;
        width: 100%;
        background-color: #2a2a2a;
        height: 100px;
        left: 0;
        z-index: 1000;
        
        padding: 0 16px;
      }
      
      .left-caption {
        width: 200px;
        
        display: flex;
        justify-content: center;
        flex-direction: start;
        height: 100%;
        align-items: center;
      }
      .right-caption {
        width: 200px;
        
        display: flex;
        justify-content: center;
        flex-direction: end;
        height: 100%;
        align-items: center;
      }
      
      
      .center-lower-panel {
        margin: auto;
        width: 500px;
        display: flex;
        justify-content: center;
        flex-direction: column;
        z-index: 2000;
      }
      
      .suwak {
        margin-top:-30px;
        margin-left:auto;
        margin-right:auto;
        margin-bottom:30px;
        width: 100%;
      }
      
      
      .app-image {
        box-shadow: none;
      
      }
      
      .our-image {
    vertical-align: middle;
    position: fixed;
    bottom: 100px;
      }
      .left-caption{
      justify-content: left;
      }
      '


nieok <- '

      .top-image {
        border-radius:5px;
        position:  absolute;
      }
      
      .image-container {
          height: 0;
          width: 20%;
          padding-bottom: 20%;
      }'