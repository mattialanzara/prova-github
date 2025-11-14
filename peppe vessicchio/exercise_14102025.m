clear all, close all, clc;

%% Exercise on ML estimator



% We're going to see...
%  a. how to generate random data for a specified model
%  b. how to implement the ML estimator for the assigned model
%  c. how to use Montecarlo simulations to estimate expected values, and
%     in particular how to use Montecarlo trials to estimate the MSE of
%     the ML estimator


%% Point a.
% Generate N data smaples from a Gaussian distribution, chose aribtrarily
% mean and variance of the distribution.
% Compute the histogram of the sampled data and compare it with the actual
% pdf of the data. Repeat the experiment by increasing the number of
% samples N, motivate what you observe.
% Write a snippet of code that generates N data samples for the model:
%  X_n = y + W_n
% where y is a deterministic value, W_n is a id noise with zero mean and
% variance equal to sigma2, distributed according to a Gaussian pdf

N = 1000;       % number of samples
y = 5;          % chosen deterministic value
sigma2 = 0.03;     % chosen variance

% now generating my own set of data...
function generate_data_det_y = generate_data_det_y(N, y, sigma2)
    W = randn(N, 1) * sqrt(sigma2);     % Generate Gaussian noise
    generate_data_det_y = y + W         % Generate data samples
end

X = generate_data_det_y(N, y, sigma2);

% now generating a gaussian pdf with mu=5 and sigma^2=2
X_i = min(X):0.01:max(X)
true_dist = normpdf(X_i, y, sqrt(sigma2))


%% Point b.
% Write a snippet of code that computes the ML estimate of theta for the
% data generated as in point a. Plot the likelihood function of the data
% and check for which value of theta it is maximized.
function mle_estimate = mle_estimate(x_i, sigma2)

    syms y real

    n = length(x_i);
    log_likelihood = 0;
    for i = 1:n
        log_likelihood = log_likelihood - (1/(2*sigma2)) * (x_i(i) - y)^2 - 0.5 * log(2*pi*sigma2); 
    end

    derivative = diff(log_likelihood, y);
    y_mle = solve(derivative == 0, y);
    mle_estimate = double(y_mle);
end

y_hat = mle_estimate(X, sigma2);
y_ML = mean(X)  % pezz8

figure;                                                                     % Create a new figure
histogram(X, 'Normalization', 'pdf');                                       % Plot histogram of sampled data
hold on;                                                                    % Hold on to overlay the pdf
plot(X_i, true_dist, 'r', 'LineWidth', 1);                                  % Plot the actual pdf
xline(y_hat, 'g', 'LineWidth', 2, 'DisplayName', 'ML Estimate');            % Plot ML estimate
xline(y_ML, 'b', 'LineWidth', 2, 'DisplayName', 'Mean Value');              % Plot mean value
xline(y, 'k--', 'LineWidth', 2, 'DisplayName', 'Real Value');               % Plot real y value
title('Histogram of Sampled Data, PDF, and Estimates');                     % Title of the plot
xlabel('X values');                                                         % X-axis label
ylabel('Probability Density');                                              % Y-axis label
legend('Sampled Data Histogram', 'Actual PDF', 'ML Estimate', 'Mean Value', 'Real Value'); % Legend
hold off;                                                                   % Release the hold on the current figure


%% Point c.
% Compute the MSE of the ML estimator by using a Montecarlo simulation.
% Check that increasing the number of Montecarlo trials the resulting
% simulated MSE becomes closer to the theoretical value sigma2/N.

samples = N;  % number of samples
sigma2_values = 1:10;  % sigma^2 values from 1 to 10
N_MC_values = [10, 100, 1000, 10000];  % number of Monte Carlo trials

figure;
hold on;

for idx = 1:length(N_MC_values)
    N_MC = N_MC_values(idx);
    mse_values = zeros(length(sigma2_values), 1);
    
    for j = 1:length(sigma2_values)
        sigma2_current = sigma2_values(j);
        y_hat_trials = zeros(N_MC, 1);
        
        % Monte Carlo simulations
        for i = 1:N_MC
            X_trial = generate_data_det_y(samples, y, sigma2_current);
            y_hat_trials(i) = mean(X_trial);
        end
        
        % Compute MSE
        mse_simulated = mean((y_hat_trials - y).^2);
        mse_values(j) = mse_simulated;
    end
    
    plot(sigma2_values, mse_values, '-o', 'LineWidth', 1.8, 'DisplayName', sprintf('N_{MC} = %d', N_MC));
end

% Theoretical MSE curve: sigma^2 / N
theoretical_mse = sigma2_values / samples;
plot(sigma2_values, theoretical_mse, 'k--', 'LineWidth', 2, 'DisplayName', '\sigma^2 / N');

xlabel('\sigma^2');
ylabel('MSE');
title('MSE of the ML estimator (Monte Carlo Simulation)');
legend('Location', 'best');
grid on;
hold off;
