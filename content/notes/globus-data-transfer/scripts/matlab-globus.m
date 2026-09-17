%% MATLAB Image Generator + Globus Transfer (with consent check)
% Configuration
src_endpoint = '<your-source-endpoint-ID>';
src_path = '<your-source-path>'; % e.g., '/Users/userID/path/to/src'
dest_endpoint = 'af187d15-768f-4449-8670-d00e1eb1ce6a';
dest_path = '<your-destination-path>'; % e.g., 'project/path/to/dest', not '/project/path/to/dest'
filename = 'generated_visualization.png';
globus_path = '</path/to/globus>';

%% Generate and save image
figure('Visible', 'off');
subplot(2,1,1), [X,Y] = meshgrid(-2:0.1:2); surf(X.*exp(-X.^2-Y.^2)), title('Surface Plot'), colorbar
subplot(2,1,2), imagesc(rand(20,20)), title('Heatmap'), colorbar, axis square
set(gcf, 'Position', [100 100 800 600]);
print(gcf, fullfile(src_path, filename), '-dpng', '-r300');

%% Transfer via Globus
cmd = sprintf('%s transfer --recursive --sync-level checksum "%s:%s" "%s:%s" --label "MATLAB Transfer"', ...
    globus_path, src_endpoint, fullfile(src_path, filename), ...
    dest_endpoint, fullfile(dest_path, filename));
[status, result] = system(cmd);

